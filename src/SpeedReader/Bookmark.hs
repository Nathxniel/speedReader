module SpeedReader.Bookmark where

import System.FilePath
import System.IO
import System.Directory (getUserDocumentsDirectory, doesFileExist)

import Control.Monad (when)

type Bookmark = Int
type BookmarkName = String

-- checks if bookmark for given filename exists
-- for given file name 
-- (e.g. eugenics.pdf checks for eugenics.pdf.bookmark)
hasBookmark :: BookmarkName -> IO Bool
hasBookmark f = getBookmarkFile f >>= doesFileExist

-- converts bookmark name (e.g. eugenics.pdf) into
-- bookmark file path from $HOME 
-- (e.g. ~/.speedReader/eugenics.pdf.bookmark)
getBookmarkFile :: BookmarkName -> IO FilePath
getBookmarkFile f = do
  home <- getUserDocumentsDirectory
  let filename = takeFileName f
  return $ home ++ "/.speedReader/" ++ filename ++ ".bookmark"

-- takes bookmark file path and creates a bookmark file
-- if one does not already exist
createBookmarkFile :: FilePath -> IO ()
createBookmarkFile f = do
  exists <- doesFileExist f
  when (not exists) $ do 
    filename <- getBookmarkFile f
    writeFile filename "0" -- create initilised bookmark file

-- writes bookmark data to bookmark's file
writeBookmark :: Bookmark -> BookmarkName -> IO ()
writeBookmark n f = do
  bookmarkFile <- getBookmarkFile f
  handle <- openFile bookmarkFile WriteMode
  hPutStrLn handle (show n)
  hClose handle

-- gets bookmark data from file at bookmark file path
getBookmarkData :: BookmarkName -> IO Bookmark
getBookmarkData f = do
  bookmark <- getBookmarkFile f
  handle <- openFile bookmark ReadMode
  (read :: String -> Int) <$> hGetContents handle
