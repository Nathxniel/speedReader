module SpeedReader.Utils where

import Data.Maybe (maybe)
import Data.Strings (strPadBoth)

import System.Console.ANSI
import qualified System.Console.Terminal.Size as Terminal
import System.IO

import Control.Monad


wpmDelay :: Float -> IO ()
-- "words per minute delay"
-- for now makes big numerical calculation
-- parametrised by input
-- to get the correct sleep to match words per minute
--
-- todo: how do you make the program wait traditionally
wpmDelay wpm = 
  (liftM (\x -> foldr (+) 0 [1..x]) bigNo) >>= doNothing
  where bigNo              = pure (d * 5000000)
        d                  = (1 / wpm) * 60
        doNothing dontcare = if dontcare > 42069
                               then return ()
                               else return ()

clearWord :: IO ()
clearWord = do
  cursorUpLine 6
  hFlush stdout

writeWord :: String -> IO ()
-- text format
writeWord w = do
  putStrLn "\n\n"
  printCentre w
  putStrLn "\n"
  
printCentre :: String -> IO ()
-- calculates the width of screen
-- creates string with width screen
-- and text in the middle
printCentre w = do
  width <- getWidth <$> Terminal.size
  putStrLn (strPadBoth ' ' width w)
  where
    getWidth = Terminal.width . (maybe (error "Terminal error") id)
