-- |
-- Module:      Main
-- Description: Grimoire generator
-- Copyright:   (c) 2019-2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Grimoire generator.
module Main
    ( main
    )
  where

import Data.Bifunctor (second)

import Hakyll hiding (pandocCompiler)
import Text.Pandoc.Definition


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

    match "js/*" do
        route idRoute
        compile copyFileCompiler

    match "templates/*"
        $ compile templateCompiler

    match "index.html" do
        route idRoute
        compile do
            articles <- loadAll "articles/*" >>= recentFirst
            projects <- loadAll "projects/*" >>= recentFirst

            let indexCtx = mconcat
                    [ listField "articles" articleCtx (pure articles)
                    , listField "projects" projectCtx (pure projects)
                    , constField "title" "Grimoire"
                    , defaultContext
                    ]

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "projects/**/*.html" do
        route idRoute
        compile copyFileCompiler

    match "projects/*.markdown" do
        route (setExtension "html")
        compile do
            pandocCompiler
                >>= loadAndApplyTemplate "templates/article.html" projectCtx
                >>= loadAndApplyTemplate "templates/default.html" projectCtx
                >>= relativizeUrls

    match "articles/*" do
        route (setExtension "html")
        compile do
            pandocCompiler
                >>= loadAndApplyTemplate "templates/article.html" articleCtx
                >>= loadAndApplyTemplate "templates/default.html" articleCtx
                >>= relativizeUrls

    create ["archive.html"] do
        route idRoute
        compile do
            articles <- loadAll "articles/*" >>= recentFirst
            let archiveCtx = mconcat
                    [ listField "articles" articleCtx (pure articles)
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

    projectCtx :: Context String
    projectCtx = defaultContext

pandocCompiler :: Compiler (Item String)
pandocCompiler = pandocCompilerWithTransform
        defaultHakyllReaderOptions
        defaultHakyllWriterOptions
        \(Pandoc meta blocks) ->
            Pandoc meta (transform <$> blocks)
  where
    transform :: Block -> Block
    transform = \case
        CodeBlock (identifier, classes, keyValue) body ->
            CodeBlock (identifier, "code-block" : classes, keyValue) body

        OrderedList attrs items ->
            OrderedList attrs (fmap transform <$> items)

        BulletList items ->
            BulletList (fmap transform <$> items)

        DefinitionList items ->
            DefinitionList (second (fmap (fmap transform)) <$> items)

        block ->
            block
