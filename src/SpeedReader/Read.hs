module SpeedReader.Read (speedRead, bookmarkRead) where

import SpeedReader.Utils
import SpeedReader.Bookmark

speedRead :: Float -> [String] -> IO ()
-- takes wpm and list of strings as arguments
speedRead _ []       = return ()
speedRead _ [w]      = writeWord w
speedRead wpm (w:ws) = do
  writeWord w
  clearWord
  wpmDelay wpm
  speedRead wpm ws 

bookmarkRead :: Bookmark -> BookmarkName -> -- bookmark data and name
                Float    -> [String]     -> IO ()
-- similar to speedRead but writes save data to "bookmark files"
bookmarkRead n f wpm ws = bmr n f wpm (skip 0 ws) 
  where skip _ []     = []
        skip pos (x:xs)
          | pos >= n  = (x:xs)
          | otherwise = skip (pos+1) xs

-- helper function for bookmarkRead
bmr :: Bookmark -> BookmarkName ->
       Float    -> [String]     -> IO ()
bmr _ _ _ []       = return ()
bmr _ f _ [w]      = writeBookmark 0 f >> writeWord w
bmr n f wpm (w:ws) = do
  writeWord w
  writeBookmark n f
  clearWord
  wpmDelay wpm
  bmr (n+1) f wpm ws
