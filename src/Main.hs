{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Main where

import           Reflex.Dom
import qualified Data.Map as Map
import           Data.Monoid ((<>))
--import Text.Highlighting.Kate
--import Text.Blaze.Html.Renderer.String (renderHtml)
--import Text.Blaze.Html (toHtml)
--import Text.Blaze.Html5 as H
--import Text.Blaze.Html5.Attributes as A hiding (value)

import           Data.Time
import           Data.Time.Clock
import           Data.Default
import           Data.FileEmbed
import           Data.String.Quote
import           Safe (readMay)
import           System.Random

--highlightHaskell :: String -> String
--highlightHaskell code = renderHtml $ toHtml
--                       $ formatHtmlBlock defaultFormatOpts
--                       $ highlightAs "haskell" code



main :: IO ()
main = do
  tStart <- getCurrentTime
  rnd    <- getStdGen
  mainWidgetWithCss $(embedFile "css/default.css") $ do
    elClass "div" "content" $ do
      demoWidget "Echo textbox contents to a div below"
        textEchoCode textEcho
      demoWidget "Display a count-up timer"
        basicTimerCode (basicTimer tStart)
      demoWidget "Count clicks on divs"
        countClicksCode countClicks
      demoWidget "Display poisson event times"
        poissonEventDemoCode (poissonEventDemo rnd tStart)
      demoWidget "Inhomogeneous poisson process"
        inhomPoissonDemoCode (inhomPoissonDemo rnd tStart)

demoWidget :: MonadWidget t m => String -> String -> m a -> m a
demoWidget descr src w = do
  elClass "div" "recipe" $ do
    elDynHtmlAttr' "div" ("class" =: "recipeHeader") $
      (constDyn descr)
    elClass "pre" "sourceCode haskell" $ do
      elDynHtmlAttr' "code" ("class" =: "sourceCode haskell")
        (constDyn src)
    el "hr" (return ())
    elClass "div" "demoWidget" w


------------------------------------------------------------------------------
textEcho :: MonadWidget t m => m ()
textEcho = do
  t <- textArea def
  el "br" (return ())
  dynText (value t)

textEchoCode :: String
textEchoCode = [s|
textEcho :: MonadWidget t m => m ()
textEcho = do
  t <- textArea def
  el "br" (return ())
  dynText (value t)
|]


------------------------------------------------------------------------------
basicTimer :: MonadWidget t m => UTCTime -> m ()
basicTimer t0 = do
  times   <- fmap (show . _tickInfo_lastUTC) <$> (tickLossy 0.2 t0)
  timeTxt <- holdDyn "No ticks yet" times
  dynText timeTxt

basicTimerCode :: String
basicTimerCode = [s|
basicTimer :: MonadWidget t m => UTCTime -> m ()
basicTimer t0 = do
  times   <- fmap (show . _tickInfo_lastUTC) <$> (tickLossy 0.2 t0)
  timeTxt <- holdDyn "No ticks yet" times
  dynText timeTxt
|]


-------------------------------------------------------------------------------
countClicks :: MonadWidget t m => m ()
countClicks = mdo

  let initialCounters = Map.fromList [(i,()) | i <- [0..2 :: Int]]
      addCounter  cs  = Map.insert (length cs) () cs
      dropCounter cs  = case Map.maxView cs of
        Nothing      -> cs
        Just (_,cs') -> cs'

  allBins <- foldDyn ($)
             initialCounters
             (leftmost $ [dropBinButton, addBinButton])

  elClass "div" "allCounts" $ do

    listWithKey allBins $ \k oneBin -> mdo

      nClicks <- foldDyn (\() -> succ) (0 :: Int) (_el_clicked boxEl)

      attrsDyn <- forDyn nClicks $ \b ->
        Map.fromList
        [("class","countBin noselect")
        ,("style","height:" ++ show (30+b*3) ++ "px;" ++
                  "background-color: hsl(" ++
                  show (b*5) ++ ",50%,50%);")]

      (boxEl,_) <- elDynAttr' "div" attrsDyn $ do
        display nClicks

      return ()

  el "br" (return ())

  -- Turn links' () click events into functions to apply to the bin map
  dropBinButton <- (fmap (\() -> dropCounter) . _link_clicked) <$>
                   linkClass "Remove Bin" "reflexLink noselect"
  addBinButton  <- (fmap (\() -> addCounter)  . _link_clicked) <$>
                   linkClass "Add Bin" "reflexLink noselect"
  return ()


countClicksCode :: String
countClicksCode = [s|
countClicks :: MonadWidget t m => m ()
countClicks = mdo

  let initialCounters = Map.fromList [(i,()) | i <- [0..2 :: Int]]
      addCounter  cs  = Map.insert (length cs) () cs
      dropCounter cs  = case Map.maxView cs of
        Nothing      -> cs
        Just (_,cs') -> cs'

  allBins <- foldDyn ($)
             initialCounters
             (leftmost $ [dropBinButton, addBinButton])

  elClass "div" "allCounts" $ do

    listWithKey allBins $ \k oneBin -> mdo

      nClicks <- foldDyn (\() -> succ) (0 :: Int) (_el_clicked boxEl)

      attrsDyn <- forDyn nClicks $ \b ->
        Map.fromList
        [("class","countBin noselect")
        ,("style","height:" ++ show (30+b*3) ++ "px;" ++
                  "background-color: hsl("++
                  show (b*5) ++ ",50%,50%);")]

      (boxEl,_) <- elDynAttr' "div" attrsDyn $ do
        display nClicks

      return ()

  el "br" (return ())

  -- Turn links' () click events into functions to apply to the bin map
  dropBinButton <- (fmap (\() -> dropCounter) . _link_clicked) <$>
                   linkClass "Remove Bin" "reflexLink noselect"
  addBinButton  <- (fmap (\() -> addCounter)  . _link_clicked) <$>
                   linkClass "Add Bin" "reflexLink noselect"
  return ()
|]

poissonEventDemo :: (RandomGen g, MonadWidget t m) => g -> UTCTime -> m ()
poissonEventDemo gen t0 = do
  ticks <- fmap (show . _tickInfo_lastUTC) <$> poissonLossy gen 10 t0
  label <- holdDyn "No ticks yet" ticks
  dynText label

poissonEventDemoCode :: String
poissonEventDemoCode = [s|
poissonEventDemo :: (RandomGen g, MonadWidget t m) => g -> UTCTime -> m ()
poissonEventDemo gen t0 = do
  ticks <- fmap (show . _tickInfo_lastUTC) <$> poissonLossy gen 10 t0
  label <- holdDyn "No ticks yet" ticks
  dynText label
|]

inhomPoissonDemo :: (RandomGen g, MonadWidget t m) => g -> UTCTime -> m ()
inhomPoissonDemo rnd t0 =  do
  rateBox  <- textInput def
  rate     <- mapDyn (maybe 1 id . readMay) (value rateBox)
  ticks    <- inhomogeneousPoisson rnd (current rate) 10 t0
  display rate
  el "br" (return ())
  let tickStrs = fmap (show . _tickInfo_lastUTC) ticks
  tickStr  <- holdDyn "No ticks yet" tickStrs
  display tickStr

inhomPoissonDemoCode :: String
inhomPoissonDemoCode = [s|
inhomPoissonDemo :: (RandomGen g, MonadWidget t m) => g -> UTCTime -> m ()
inhomPoissonDemo rnd t0 =  do
  rateBox  <- textInput def
  rate     <- mapDyn (maybe 1 id . readMay) (value rateBox)
  ticks    <- inhomogeneousPoisson rnd (current rate) 10 t0
  display rate
  el "br" (return ())
  let tickStrs = fmap (show . _tickInfo_lastUTC) ticks
  tickStr  <- holdDyn "No ticks yet" tickStrs
  display tickStr
|]
