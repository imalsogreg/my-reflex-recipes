{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Main where

import           Control.Concurrent
import           Control.Monad (liftM)
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
import           GHCJS.DOM
import           GHCJS.DOM.Document
import           GHCJS.DOM.Element
import           GHCJS.DOM.Document
import           GHCJS.DOM.HTMLElement
import           GHCJS.DOM.HTMLDocument

--highlightHaskell :: String -> String
--highlightHaskell code = renderHtml $ toHtml
--                       $ formatHtmlBlock defaultFormatOpts
--                       $ highlightAs "haskell" code

------------------------------------------------------------------------------
waitUntilJust :: IO (Maybe a) -> IO a
waitUntilJust a = do
    mx <- a
    case mx of
      Just x -> return x
      Nothing -> do
        threadDelay 10000
        waitUntilJust a


main :: IO ()
main = do
  tStart <- getCurrentTime
  rnd    <- getStdGen
  runWebGUI $ \webView -> do
    doc <- waitUntilJust $ liftM (fmap castToHTMLDocument) $
           webViewGetDomDocument webView
    let btag = "reflex-area" :: String
    root <- waitUntilJust $ liftM (fmap castToHTMLElement) $
            documentGetElementById doc btag
    attachWidget root webView (runApp tStart rnd)

runApp :: forall t m g.(MonadWidget t m, RandomGen g) => UTCTime -> g -> m ()
runApp tStart rnd = do
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
      demoWidget "Bouncing Ball"
        "in progress" (demoBounce tStart)
      demoWidget "Whack-a-Mole"
        "in progress" (whackAMole rnd tStart)

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
  ticks <- fmap (show) <$> poissonLossy gen 10 t0
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
  let tickStrs = fmap (show) ticks
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
  let tickStrs = fmap (show) ticks
  tickStr  <- holdDyn "No ticks yet" tickStrs
  display tickStr
|]

demoBounce :: MonadWidget t m => UTCTime -> m ()
demoBounce = text "Is it possible?" & return

whackAMole :: (RandomGen g, MonadWidget t m) => g -> UTCTime -> m ()
whackAMole rnd t0 = mdo
  let (r,r') = split rnd
  let (r'',r''') = split r'
  moleWidget r t0 (constDyn 0.5)
  el "br" (return ())
  moleWidget r' t0 (constDyn 0.2)
  el "br" (return ())
  moleWidget r'' t0 (constDyn 0.5)
  el "br" (return ())
  moleWidget r''' t0 (constDyn 0.1)
  return ()

-- moleWidget handles one active mole, triggering internal pop-up events and returning
-- events that signal a successfull whack or unsuccessful appear-disappear sequences
moleWidget :: (RandomGen g, MonadWidget t m)
           => g
           -> UTCTime
           -> Dynamic t Double
           -> m (Event t MoleState)
moleWidget rnd t0 popupRate = mdo

  --picAttrs    <- forDyn moleState $ \s -> mappend (pSrc s) ["class" =: "mole-pic"]
  let pSrc s  = case s of
                 MoleUp      -> "src" =: "MoleUp.png"
                 MoleDown    -> "src" =: "MoleDown.png"
                 MoleWhacked -> "src" =: "MoleWhacked.png"
  --molePic     <- elDynAttr "img" picAttrs
  molePic     <- forDyn moleState show >>= elDynHtml' "div"
  let (r,r')  =  split rnd

  popupTimes  <- inhomogeneousPoisson r (current popupRate) 5 t0
  goDownTimes <- mapDyn (*2) popupRate >>= \goDownRate ->
                 inhomogeneousPoisson r' (current goDownRate) 10 t0
  let whacks  =  _el_clicked molePic
  moleState   <- foldDyn (\e s -> updateMole e s) MoleDown
                 (leftmost [fmap (const GoUp)   popupTimes
                           ,fmap (const GoDown) goDownTimes
                           ,fmap (const Whack)  whacks
                           ])
  return . ffilter (== MoleWhacked) . updated $ moleState


updateMole :: MoleAction -> MoleState -> MoleState
updateMole GoUp   MoleDown    = MoleUp
updateMole GoDown MoleUp      = MoleDown
updateMole Whack  MoleUp      = MoleWhacked
updateMole GoDown MoleWhacked = MoleDown
updateMole _ s                 = s

data MoleState = MoleDown | MoleUp | MoleWhacked
               deriving (Eq, Show)

data MoleAction = Whack | GoUp | GoDown
