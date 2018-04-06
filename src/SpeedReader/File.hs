module SpeedReader.File (processFile, processPDF, createAudiobookWAV) where

import System.Process  ( StdStream(..)
                       , createProcess 
                       , CreateProcess(..)
                       , proc
                       , shell
                       , callCommand
                       )
import System.IO (hGetContents, hClose)

data FileType = PDF | TXT

getFileType :: FilePath -> FileType
getFileType fp =
  case extension of
    ['p','d','f'] -> PDF
    ['t','x','t'] -> TXT
    x                 -> error $ "file type " ++ x ++ " not recognised"
    where (e, f) = break (=='.') $ reverse fp
          (filename, extension) = (reverse f, reverse e)

processFile :: FilePath -> IO String
processFile fp =
  case getFileType fp of
    PDF -> do
             (_, Just hout, _, _) <- createProcess $
               (proc "pdftotext" ["-nopgbrk"
                                 ,"-q"
                                 ,fp
                                 ,"-"
                                 ]
               ) { cwd     = Nothing
                 , std_out = CreatePipe }
             out <- hGetContents hout
             return out
    TXT -> readFile fp

-- allows user to specify first and last page of a pdf
processPDF :: FilePath -> String -> String -> IO String
processPDF fp f l = do
  (_, Just hout, _, _) <- createProcess $
    (proc "pdftotext" ["-f"
                      ,f
                      ,"-l"
                      ,l
                      ,"-nopgbrk"
                      ,"-q"
                      ,fp
                      ,"-"
                      ]
    ) { cwd     = Nothing
      , std_out = CreatePipe }
  out <- hGetContents hout
  return out

createAudiobookWAV :: [String] -> IO ()
createAudiobookWAV (x:xs) = do
  putStrLn "Creating audiobook.. (please wait, this may take a while)"
  callCommand $ "echo make | zipwav " ++ x
  createAudiobookWAV' xs

createAudiobookWAV' []     = pure ()
createAudiobookWAV' (s:ss) = do
  callCommand $ "echo nomake | zipwav \"" ++ s ++ "\""
  createAudiobookWAV' ss
