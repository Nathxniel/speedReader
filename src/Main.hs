module Main where

import System.Environment (getArgs)
import System.IO (hGetContents, stdin)

import SpeedReader.Utils
import SpeedReader.File
import SpeedReader.Bookmark
import SpeedReader.Read

main :: IO ()
  -- takes in a text or pdf file
  -- finds all the words
  -- shows the words on screen at given speed
main = do
  args <- getArgs
  case args of
    "-h":_            -> usage
    ["-c",words,pages] -> putStrLn $ progress words pages
    [wpm, file, f, l] -> processPDF file f l >>= speedRead (read' wpm)
    [wpm, file]       -> do
                           -- do the bookmark processing
                           --   if bookmark exists, start from there
                           --   else start from 0
                           exists <- hasBookmark file
                           n <- if exists
                                  then getBookmarkData file
                                  else createBookmarkFile file >> return 0 
                           putStrLn . unwords $ ["starting from"
                                                ,show . show $ n]
                           ws <- processFile file
                           bookmarkRead n file (read' wpm) ws
    [wpm]             -> catInput >>= speedRead (read' wpm)
    _                 -> usage
    where read'    = read :: String -> Float
          catInput = words <$> hGetContents stdin
          progress w p = ("about " ++) 
                         . (++ "% done") 
                         . show 
                         . round 
                         $ approxprog w p
          approxprog w p = 100 * ((read' w) / (300 * (read' p))) -- *
          -- * there are approx 300 pages per minute

usage :: IO ()
usage =
  putStrLn . unlines $
  ["usage; program used in the following ways:"
  ,"  1) $ speedReader wpm file"
  ,"  2) $ speedReader wpm pdf_file firstpage lastpage"
  ,"  3) $ cat file | speedReader wpm"
  ,"  4) $ speedReader -h"
  ,"  5) $ speedReader -c bookmark pages"
  ,""
  ,"  1 - reads pdf or txt file at wpm words per minute"
  ,"  2 - reads a section of a file at wpm words per minute (pdf only)"
  ,"  3 - reads input from stdin at wpm words per minute"
  ,"  4 - shows this usage"
  ,"  5 - calculates approximate progress in book"
  ,""
  ,"  Note: bookmark data is only saved when using program as per 1)"
  ,"  (bookmarks save where quitted the program/stopped reading)"
  ]
