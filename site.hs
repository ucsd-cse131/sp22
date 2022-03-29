--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mappend)
import Hakyll
import Text.Pandoc

crunchWithCtx ctx = do
  route   $ setExtension "html"
  compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/page.html"    ctx
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls



--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
  match "static/*/*"    $ do route idRoute
                             compile copyFileCompiler
  match (fromList tops) $ crunchWithCtx siteCtx 
  match "lectures/*"    $ crunchWithCtx postCtx 
  match "assignments/*" $ crunchWithCtx postCtx 
  match "templates/*"   $ compile templateCompiler
 
--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField  "date"       "%B %e, %Y"  `mappend`
    -- constField "headerImg"  "Eiffel.jpg" `mappend`
    siteCtx

siteCtx :: Context String
siteCtx =
    constField "baseurl"            "https://ucsd-cse131.github.io/sp22"     `mappend`
    constField "site_name"          "cse131"                    `mappend`
    constField "site_description"   "UCSD CSE 131"              `mappend`
    constField "site_username"      "Ranjit Jhala"              `mappend`
    constField "twitter_username"   "ranjitjhala"               `mappend`
    constField "github_username"    "ucsd-cse131/sp22"          `mappend`
    constField "google_username"    "rjhala@eng.ucsd.edu"       `mappend`
    constField "google_userid"      "u/0/106612421534244742464" `mappend`
    constField "piazza_id"          "ucsd/spring2022/cse131/home" `mappend`
    constField "canvas_id"          "FIXME" `mappend`
    defaultContext

tops =
  [ "index.md"
  , "grades.md"
  , "lectures.md"
  , "links.md"
  , "assignments.md"
  , "faq.md"
  , "contact.md"
  ]
