{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}


module Main where
import Reflex.Dom
import Data.Time
import Data.Time.Clock
import Data.Default
import Data.FileEmbed
import Data.String.Quote


main :: IO ()
main = do
  tStart <- getCurrentTime
  mainWidgetWithCss $(embedFile "css/default.css") $ do
    elClass "div" "content" $ do
      demoWidget "Echo textbox contents to a div below"
        textEchoCode textEcho
      demoWidget "Display a count-up timer"
        basicTimerCode (basicTimer tStart)
      return ()

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

basicTimer :: MonadWidget t m => UTCTime -> m ()
basicTimer t0 = do
  times <- fmap (show . _tickInfo_lastUTC) <$> (tickLossy 0.2 t0)
  timeDyn <- holdDyn "No ticks yet" times
  dynText timeDyn

basicTimerCode :: String
basicTimerCode = [s|
basicTimer :: MonadWidget t m => UTCTime -> m ()
basicTimer t0 = do
  times <- fmap (show . _tickInfo_lastUTC) <$> (tickLossy 0.2 t0)
  timeDyn <- holdDyn "No ticks yet" times
  dynText timeDyn
|]
