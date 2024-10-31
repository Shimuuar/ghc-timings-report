{-# LANGUAGE OverloadedStrings #-}
-- |
-- Generate reports from 
module GhcTimings.Report where

import Colonnade
import Control.Lens
import Data.Foldable
import Data.String
import Data.Ord                         (Down(..))
import Data.List.Split                  (chunksOf)
import Data.List                        (intercalate,sortOn)
import Data.Map.Strict                  (Map)
import Data.Map.Strict                  qualified as Map
import Data.Text                        (Text)
import Data.Text                        qualified as T
import Data.Text.Lazy.Builder           (Builder)
import Data.Text.Lazy.Builder.RealFloat (formatRealFloat,FPFormat(..))
import Text.Blaze.Colonnade
import Text.Blaze.Html5 hiding (map, head)
import Text.Blaze.Html5            qualified as H
-- import Text.Blaze.Html5.Attributes hiding (title, rows, accept)
import Text.Blaze.Html5.Attributes qualified as A

import GhcTimings.Types

index :: [Package ([Phase], Map Text [Phase])] -> Markup
index packages = docTypeHtml $ do
  H.head $ do
    title "GHC timings report"
    H.link ! A.rel "stylesheet"
           ! A.type_ "text/css"
           ! A.href "./main.css"
  body $ do
    h1 "Packages"
    let tbl = TimeTable
          { title    = "Module"
          , totAlloc = sumOf (each . to (.components) . each . allPhases . to (.alloc))
              packages
          , totTime  = sumOf (each . to (.components) . each . allPhases . to (.time))
              packages
          , getName  = view _1
          , getAlloc = view _2
          , getTime  = view _3
          }
    for_ packages $ \pkg -> do
      h2 $ a ! A.href (fromString $ "./" <> pkg.name <> ".html")
         $ toMarkup pkg.name
      encodeTimeTable tbl mempty
        [ ( toMarkup $ show nm
          , sumOf (allPhases . to (.alloc)) comp
          , sumOf (allPhases . to (.time))  comp
          )
        | (nm,comp) <- Map.toList pkg.components
        ]


package :: Package ([Phase], Map Text [Phase]) -> Markup
package pkg = docTypeHtml $ do
  H.head $ do
    title "GHC timings report"
    H.link ! A.rel "stylesheet"
           ! A.type_ "text/css"
           ! A.href "./main.css"
  --
  body $ do
    h1 $ fromString pkg.name
    p  $ H.a ! A.href "./index.html" $ "index"
    -- Per-module time/allocation
    for_ (Map.toList pkg.components) $ \(name, all_mods@(no_mod,mods)) -> do
      h2 $ fromString $ show name
      let tbl = TimeTable
            { title    = "Module"
            , totAlloc = sumOf (allPhases . to (.alloc)) all_mods
            , totTime  = sumOf (allPhases . to (.time))  all_mods
            , getName  = view _1
            , getAlloc = view _2
            , getTime  = view _3
            }
      encodeTimeTable tbl mempty
        [ ( ( if nm == "-"
              then id
              else H.a ! A.href (textValue $ nm <> ".html"))
            $ toMarkup nm
          , sumOf (each . to (.alloc)) row
          , sumOf (each . to (.time))  row
          )
        | (nm,row) <- ("-",no_mod) : Map.toList mods
        ]

modulePage :: Text -> [Phase] -> Markup
modulePage mod_name phases = do
  H.head $ do
    title $ toMarkup $ "GHC timings report: " <> mod_name
    H.link ! A.rel "stylesheet"
           ! A.type_ "text/css"
           ! A.href "./main.css"
  body $ do
    h1 $ toMarkup mod_name
    h2 $ "Phases"
    encodeTimeTable tbl1
      (headed (headerCell "N") (numberCell . toMarkup . fst))
      ([1::Int ..] `zip` phases)
    h2 $ "By phase"
    encodeTimeTable tbl2 mempty
      ( sortOn (\(_,(_,t)) -> Down t)
      $ Map.toList
      $ Map.fromListWith (\(a1,t1) (a2,t2) -> (a1+a2,t1+t2))
        [ (p.name, (p.alloc, p.time))
        | p <- phases
        ]
      )
  where
    tbl1 = TimeTable
      { title    = "Phase"
      , totAlloc = sumOf (each . to (.alloc)) phases
      , totTime  = sumOf (each . to (.time))  phases
      , getName  = toMarkup . (.name) . snd
      , getAlloc = (.alloc) . snd
      , getTime  = (.time)  . snd
      }
    tbl2 = TimeTable
      { title    = "Phase"
      , totAlloc = sumOf (each . to (.alloc)) phases
      , totTime  = sumOf (each . to (.time))  phases
      , getName  = toMarkup . view _1
      , getAlloc = view (_2 . _1)
      , getTime  = view (_2 . _2)
      }


data TimeTable a = TimeTable
  { title    :: !Text
  , totAlloc :: !Int
  , totTime  :: !Double
  , getName  :: a -> Html
  , getAlloc :: a -> Int
  , getTime  :: a -> Double
  }

encodeTimeTable :: TimeTable a -> Colonnade Headed a Cell -> [a] -> Html
encodeTimeTable tbl prefix
  = encodeCellTable mempty
  $ mconcat
    [ prefix
    , headed (headerCell tbl.title)
             (moduleCell . tbl.getName)
    , headed (headerCell "ALLOC")
        (numberCell . formatBigInt . tbl.getAlloc)
    , headed (headerCell "(%)")
        (\x -> numberCell $ toMarkup
          $ formatF1 (100 * fromIntegral (tbl.getAlloc x) / tot_alloc) <> "%")
    , headed (headerCell "Time (ms)")
        (\x -> numberCell $ toMarkup $ formatF1 (tbl.getTime x) <> "ms")
    , headed (headerCell "(%)")
        (\x -> numberCell $ toMarkup
             $ formatF1 (100 * (tbl.getTime x) / tot_time) <> "%" )
    ]
  where
    tot_alloc = fromIntegral tbl.totAlloc :: Double
    tot_time  = tbl.totTime               :: Double

----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

moduleCell,numberCell :: Html -> Cell
moduleCell x = (htmlCell x){ cellAttribute = A.class_ "module" }
numberCell x = (htmlCell x){ cellAttribute = A.class_ "number" }

headerCell :: Text -> Cell
headerCell x = (textCell x){ cellAttribute = A.scope "col" <> A.class_ "header"
                           }

formatBigInt :: Int -> Markup
formatBigInt = preEscapedToMarkup . reverse . intercalate (reverse "&nbsp;") . chunksOf 3 . reverse . show 

formatF1 :: Double -> Builder
formatF1 = formatRealFloat Fixed (Just 1)


