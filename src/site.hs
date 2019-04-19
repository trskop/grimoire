-- |
-- Module:      Main
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2019 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- TODO: Module description.
module Main
    ( main
    )
  where

import Data.Monoid (mconcat)

import Hakyll


config :: Configuration
config = defaultConfiguration
    { deployCommand = "bash -ex ./deploy.bash"
    }

main :: IO ()
main = hakyllWith config do
    match "images/*" do
        route idRoute
        compile copyFileCompiler

    match "css/*" do
        route idRoute
        compile compressCssCompiler

    match "templates/*"
        $ compile templateCompiler

    match "index.html" do
        route idRoute
        compile do
            articles <- loadAll "articles/*" >>= recentFirst
            let indexCtx = mconcat
                    [ listField "articles" articleCtx (return articles)
                    , constField "title" "Grimoire"
                    , defaultContext
                    ]

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "articles/*" do
        route (setExtension "html")
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/article.html" articleCtx
            >>= loadAndApplyTemplate "templates/default.html" articleCtx
            >>= relativizeUrls

    create ["archive.html"] do
        route idRoute
        compile do
            articles <- loadAll "articles/*" >>= recentFirst
            let archiveCtx = mconcat
                    [ listField "articles" articleCtx (return articles)
                    , constField "title" "Spell Archive"
                    , defaultContext
                    ]

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls
  where
    articleCtx :: Context String
    articleCtx = mconcat
        [ dateField "date" "%d %B %Y"
        , defaultContext
        ]
