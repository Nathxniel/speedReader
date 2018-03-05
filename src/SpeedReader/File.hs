module SpeedReader.File (processFile, processPDF) where

import System.Process  ( StdStream(..)
                       , createProcess 
                       , CreateProcess(..)
                       , proc
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

processFile :: FilePath -> IO [String]
-- takes filename and converts to [String]
-- uses pdftotext -nopgbrk -q
-- 
-- if file is text, just get it
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
             return (words out)
    TXT -> words <$> readFile fp

-- allows user to specify first and last page of a pdf
processPDF :: FilePath -> String -> String -> IO [String]
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
  return (words out)
