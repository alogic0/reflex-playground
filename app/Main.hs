{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeFamilies        #-}

module Main where

import           Control.Concurrent
import           Control.Monad (liftM, forM)
import           Reflex
import           Reflex.Dom
import           Reflex.Dom.Widget
import           GHCJS.DOM.Document
import           GHCJS.DOM.HTMLElement
import           Data.Time
import           Data.Time.Clock
import qualified Data.Map as Map
import           System.Random
import           Data.String.Quote

--------------------------------------------------------------------------------
      
main :: IO ()
main = do
  tStart <- getCurrentTime
  rnd    <- getStdGen
  mainWidget $ mainUI tStart

mainUI :: (MonadWidget t m) => UTCTime -> m ()
mainUI tStart = do
  navbar
  divClass "container-fluid" $ examples tStart

navbar :: (MonadWidget t m) =>  m ()
navbar = do
  elClass "nav" "navbar navbar-default navbar-static-top" $ do
    divClass "container-fluid"  $ do
      divClass "navbar-header"  $ do
        divClass "navbar-brand" $ do
          text "Pollock Reflex FrontEnd"

examples :: (MonadWidget t m) =>  UTCTime -> m ()
examples tStart = do
  divClass "row" $ do
    divClass "col-md-4" $ do
      text ""
    divClass "col-md-5" $ do
      divClass "" example1
      divClass "" (example2 tStart)
      divClass "" example3 
    divClass "col-md-4" $ do
      text ""

example1 :: (MonadWidget t m) => m ()
example1 = do
  demoWidget "Example1: TextBox Echo"
    textEchoCode textEcho

example2 :: (MonadWidget t m) => UTCTime -> m ()
example2 tStart = do
  demoWidget "Example2: CountDown"
    basicTimerCode (basicTimer tStart)

example3 :: (MonadWidget t m) =>  m ()
example3 = do
  demoWidget "Count clicks"
    countClicksCode countClicks

demoWidget :: MonadWidget t m => String -> String -> m a -> m a
demoWidget descr src w = do
  elClass "div" "recipe" $ do
    elDynHtmlAttr' "div" ("class" =: "recipe-header") $
      (constDyn descr)
    elClass "pre" "sourceCode haskell" $ do
      elDynHtmlAttr' "code" ("class" =: "sourceCode haskell")
        (constDyn src)
--    el "hr" (return ())
    elClass "div" "demoWidget" w
    
      
textEcho :: MonadWidget t m => m ()
textEcho = do
  let cfg  = TextAreaConfig "" never (constDyn ( "class" =: "inputee")) 
  t <- textArea cfg
  el "br" (return ())
  elClass "div" "output" $ do
    dynText (value t)

textEchoCode :: String
textEchoCode =
  [s|
  textEcho :: MonadWidget t m => m ()
  textEcho = do
    let cfg  = TextAreaConfig "" never (constDyn ( "class" =: "inputee")) 
    t <- textArea cfg
    el "br" (return ())
    elClass "div" "output" $ do
      dynText (value t)
  |]

basicTimer :: MonadWidget t m => UTCTime -> m ()
basicTimer t0 = do
  times   <- fmap (show . _tickInfo_lastUTC) <$> (tickLossy 0.2 t0)
  timeTxt <- holdDyn "No ticks yet" times
  elClass "div" "output" $ do
    dynText timeTxt

basicTimerCode :: String
basicTimerCode =
  [s|
  basicTimer :: MonadWidget t m => UTCTime -> m ()
  basicTimer t0 = do
    times   <- fmap (show . _tickInfo_lastUTC) <$> (tickLossy 0.2 t0)
    timeTxt <- holdDyn "No ticks yet" times
    elClass "div" "output" $ do
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
                   linkClass "Remove " "reflexLink noselect"
  addBinButton  <- (fmap (\() -> addCounter)  . _link_clicked) <$>
                   linkClass " Add" "reflexLink noselect"
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

