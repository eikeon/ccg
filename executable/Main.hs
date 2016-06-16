{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Maybe                       (mapMaybe)
import           CCG.Category
import           CCG.Parser
import           System.IO
import qualified Text.Blaze.Html.Renderer.Pretty  as HPP
import qualified Text.Blaze.Html5                 as H
import qualified Text.Blaze.Html5.Attributes      as A
import qualified Data.Map as M
import           Control.Monad

import Morte.Core (Const(..), Expr(..))


html :: H.Html -> H.Html
html content = H.docTypeHtml $ do
    H.head $ do
        H.title "result"
        H.style "* {padding:0; margin: 0} .root{padding:20px} .word{padding:0px 10px 0px 10px} hr{margin-bottom:3px} .category{display:inline-block; padding-bottom: 11px; text-align:center; vertical-align:top}"
        H.meta H.! A.charset "UTF-8"
    H.body content

showDerivation :: CellData -> Maybe H.Html
-- TODO: target category an argument
showDerivation cellData@(CellData (Category "S") _) = Just $ H.div H.! A.class_ "root" $ showParsePath cellData
showDerivation _ = Nothing

showParsePath :: CellData -> H.Html
showParsePath (CellData cat (ParsePath left right rule)) =
    H.div H.! A.class_ "category" $ do
        showParsePath left
        showParsePath right
        H.hr
        H.toHtml $ show cat
        H.span " ("
        H.toHtml $ show rule
        H.span  " )"
showParsePath (CellData cat (ParseWord word)) = H.div H.! A.class_ "word category" $ do
        H.toHtml word
        H.hr
        H.toHtml $ show cat

chartToHtml :: Chart -> H.Html
chartToHtml (Cell datas _ _ _) = forM_ showDerivations H.div H.! A.class_ "derivation"
  where showDerivations = mapMaybe showDerivation datas
chartToHtml End = H.div "end"


main :: IO ()
main = do
    let lexicon = M.fromList [
            ("I", [(Category "NP", Const Star)])
            , ("dislike", [((Category "S" :\ Category "NP") :/ Category "NP", Const Star)])
            , ("and", [(Category "C", Const Star)])
            , ("Mary", [(Category "NP", Const Star)])
            , ("likes", [((Category "S" :\ Category "NP") :/ Category "NP", Const Star)])
            , ("musicals", [(Category "NP", Const Star)])
            ]
    let sample = parse defaultCombinatorsSet lexicon "I dislike and Mary likes musicals"
    file <- openFile "output.html" WriteMode
    hPutStr file $ HPP.renderHtml $ (html . chartToHtml) sample
    hClose file
