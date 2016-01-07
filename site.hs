--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid ((<>))
import           Hakyll
import           Hakyll.Web.Tags

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" fullContext
            >>= relativizeUrls

    tags <- buildTags "posts/*" (fromCapture "tags/*.html")
    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged \"" ++ tag ++ "\""
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = constField "title" title
                      <> listField "posts" postCtx (return posts)
                      <> postCtx
            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    (postCtxWithTags tags)
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" (postCtxWithTags tags)
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" (postCtxWithTags tags) (return posts)
                    <> constField "title" "Archives"
                    <> fullContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" (postCtxWithTags tags) (return posts)
                    <> constField "title" "Home"
                    <> fullContext
                    <> field "isHome" (\_ -> return "yes")

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

    let renderFeed path renderer =
          create path $ do
            route idRoute
            compile $ do
                let feedCtx = postCtx `mappend` bodyField "description"
                posts <- fmap (take 10) . recentFirst =<<
                    loadAllSnapshots "posts/*" "content"
                renderer (myFeedConfiguration "All posts") feedCtx posts

    renderFeed ["feed/atom.xml"] renderAtom
    renderFeed ["feed/rss.xml"] renderRss

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx = fullContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags <> postCtx

fullContext :: Context String
fullContext =
  field "siteTitle" (\_ -> return "Lennart Kolmodin")
  <> dateField "date" "%B %e, %Y"
  <> defaultContext

myFeedConfiguration :: String -> FeedConfiguration
myFeedConfiguration title = FeedConfiguration
    { feedTitle       = "Lennart Kolmodin - " ++ title
    , feedDescription = "Lennart Kolmodin's blog"
    , feedAuthorName  = "Lennart Kolmodin"
    , feedAuthorEmail = "kolmodin@gmail.com"
    , feedRoot        = "https://kolmodin.github.io"
    }
