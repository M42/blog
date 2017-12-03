--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Text.Pandoc.Options
import qualified Data.Set as S
import           Hakyll
import qualified Data.ByteString.Lazy    as LB
import           Text.Regex
import           Debug.Trace

--------------------------------------------------------------------------------
conf :: Configuration
conf = def { destinationDirectory = "docs" }

myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "Logoi"
    , feedDescription = "Mathematics, logic, and computer science"
    , feedAuthorName  = "Mario Román"
    , feedAuthorEmail = "test@example.com"
    , feedRoot        = "http://m42.github.io/blog"
    }


main :: IO ()
main = hakyllWith conf $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.org","works.org"]) $ do
        route   $ setExtension "html"
        compile $ pandocMathCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocMathCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    -- match "talks/*.org" $ do
    --     route $ setExtension "pdf"
    --     compile $ getResourceLBS >>= withItemBody (unixFilterLBS "org2beamer" ["-d"])

    create ["index.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Posts"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    -- create ["talks.html"] $ do
    --     route idRoute
    --     compile $ do
    --         talks <- recentFirst =<< byteLoader (loadAll "talks/*.org")
    --         let archiveCtx =
    --                 listField "posts" postCtx (return talks) `mappend`
    --                 constField "title" "Talks"            `mappend`
    --                 defaultContext

    --         makeItem ""
    --             >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
    --             >>= loadAndApplyTemplate "templates/default.html" archiveCtx
    --             >>= relativizeUrls  

    match "templates/*" $ compile templateBodyCompiler

    create ["atom.xml"] $ do
      route idRoute
      compile $ do
        let feedCtx = postCtx `mappend` bodyField "description"

        posts <- fmap (take 10) . recentFirst =<<
          loadAllSnapshots "posts/*" "content"
        renderAtom myFeedConfiguration feedCtx posts


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" `mappend` defaultContext

--------------------------------------------------------------------------------
-- Compilers
-- Mathjax.
-- http://travis.athougies.net/posts/2013-08-13-using-math-on-your-hakyll-blog.html
pandocMathCompiler =
    let mathExtensions = [Ext_tex_math_dollars, Ext_tex_math_double_backslash,
                          Ext_latex_macros]
        defaultExtensions = writerExtensions defaultHakyllWriterOptions
        newExtensions = foldr S.insert defaultExtensions mathExtensions
        writerOptions = defaultHakyllWriterOptions {
                          writerExtensions = newExtensions,
                          writerHTMLMathMethod = MathJax ""
                        }
    in pandocCompilerWith defaultHakyllReaderOptions writerOptions


-- byteLoader :: Compiler [Item LB.ByteString] -> Compiler [Item String]
-- byteLoader = fmap (fmap (fmap ((fmap toEnum) . (fmap fromEnum) . LB.unpack)))
