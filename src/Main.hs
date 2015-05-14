{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}


module Main where

import           Reflex.Dom
import qualified Data.Map as Map
import           Data.Monoid ((<>))
import           Data.Time
import           Data.Time.Clock
import           Data.Default
import           Data.FileEmbed
import           Data.String.Quote


main :: IO ()
main = do
  tStart <- getCurrentTime
  mainWidgetWithCss $(embedFile "css/default.css") $ do
    elClass "div" "content" $ do
      demoWidget "Echo textbox contents to a div below"
        textEchoCode textEcho
      demoWidget "Display a count-up timer"
        basicTimerCode (basicTimer tStart)
      demoWidget "Count clicks on divs"
        countClicksCode countClicks

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

  let initialCounters = Map.fromList [(i,0 :: Int) | i <- [0..2 :: Int]]
      addCounter  cs  = Map.insert (length cs) 0 cs
      dropCounter cs  = case Map.maxView cs of
        Nothing      -> cs
        Just (_,cs') -> cs'
      incrAtKey cs k  = case Map.lookup k of
        Nothing -> cs
        Just v  -> Map.insert k (succ v) cs

  allBins <- foldDyn ($)
             initialCounters (leftmost $ [dropBinButton, addBinButton] ++ boxClicks)

  elClass "div" "allCounts" $ do

    boxEls <- listWithKey allBins $ \k oneBin -> do
      boxEl <- forDyn oneBin $ \b ->
        Map.fromList [("class","countBin")
                     ,("style","background-color:blue; width: 10px; float:left;")
                     ,("height", show (10 + b) <> "px")
                     ]
      fullBoxEl <- elDynAttr' "div" boxAttrs $ do
          display oneBin
      return fullBoxEl

    let boxClicked = (fmap (\() -> incrAtKey k) . _el_clicked) <$> boxAttrs
    return boxClicked

  el "br" (return ())

  -- Turn links' () click events into functions to apply to the bin map
  dropBinButton <- (fmap (\() -> dropCounter) . _link_clicked) <$>
                   link "Remove Bin"
  addBinButton  <- (fmap (\() -> addCounter)  . _link_clicked) <$>
                   link "Add Bin"

  return ()


countClicksCode :: String
countClicksCode = "In progress"
