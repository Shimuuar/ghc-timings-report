{-# LANGUAGE OverloadedStrings #-}
-- |
-- Generate reports from 
module GhcTimings.Report where

import Colonnade
import Control.Lens
import Data.Foldable
import Data.String
import Data.List.Split                  (chunksOf)
import Data.List                        (intercalate)
import Data.Map.Strict                  (Map)
import Data.Map.Strict                  qualified as Map
import Data.Text                        (Text)
import Data.Text.Lazy.Builder           (Builder)
import Data.Text.Lazy.Builder.RealFloat (formatRealFloat,FPFormat(..))
import Text.Blaze.Colonnade
import Text.Blaze.Html5 hiding (map, head)
import Text.Blaze.Html5            qualified as H
-- import Text.Blaze.Html5.Attributes hiding (title, rows, accept)
import Text.Blaze.Html5.Attributes qualified as A

import GhcTimings.Types

index :: [Package a] -> Markup -- TODO: use T.Text
index packages = docTypeHtml $ do
  H.head $ do
    title "GHC timings report"
    H.link ! A.rel "stylesheet"
           ! A.type_ "text/css"
           ! A.href "./main.css"
  body ! A.style "width: 500px; margin: 0px auto 0px auto;" $ do
    h1 "Packages"
    p $ do
      "List of the packages in the project"
    ul $ for_ packages $ \pkg ->
         li
            $ a ! A.href (fromString $ "./" <> pkg.name <> ".html")
                $ toMarkup pkg.name


package :: Package ([Phase], Map Text [Phase]) -> Markup
package pkg = docTypeHtml $ do
  H.head $ do
    title "GHC timings report"
    H.link ! A.rel "stylesheet"
           ! A.type_ "text/css"
           ! A.href "./main.css"
  --
  h1 $ fromString pkg.name
  p  $ H.a ! A.href "./index.html" $ "index"
  -- Per-module time/allocation
  for_ (Map.toList pkg.components) $ \(name,(no_mod,mods)) -> do
    h2 $ fromString $ show name
    let tot_alloc = sumOf (each .        to (.alloc)) no_mod
                  + sumOf (each . each . to (.alloc)) mods
        tot_time  = sumOf (each .        to (.time))  no_mod
                  + sumOf (each . each . to (.time))  mods
    encodeCellTable mempty
      (mconcat
       [ headed (headerCell "Module")
           (\(nm,_,_) -> moduleCell $ toMarkup nm)
       , headed (headerCell "ALLOC")
           (\(_,alloc,_) -> numberCell $ toMarkup $ formatBigInt alloc)
       , headed (headerCell "(%)")
           (\(_,alloc,_) -> numberCell $ toMarkup
             $ formatF1 (100 * fromIntegral alloc / fromIntegral tot_alloc) <> "%")
       , headed (headerCell "Time (ms)")
           (\(_,_,time) -> numberCell $ toMarkup
                         $ formatF1 time <> "ms")
       , headed (headerCell "(%)")
           (\(_,_,time) -> numberCell $ toMarkup
                         $ formatF1 (100 * time / tot_time) <> "%" )
       ])
      [ ( nm
        , sumOf (each . to (.alloc)) row
        , sumOf (each . to (.time))  row
        )
      | (nm,row) <- ("-",no_mod) : Map.toList mods
      ]

moduleCell,numberCell :: Html -> Cell
moduleCell x = (htmlCell x){ cellAttribute = A.class_ "module" }
numberCell x = (htmlCell x){ cellAttribute = A.class_ "number" }

headerCell :: Text -> Cell
headerCell x = (textCell x){ cellAttribute = A.scope "col" <> A.class_ "header"
                           }




formatBigInt :: Int -> String
formatBigInt = reverse . intercalate " " . chunksOf 3 . reverse . show 

formatF1 :: Double -> Builder
formatF1 = formatRealFloat Fixed (Just 1)
