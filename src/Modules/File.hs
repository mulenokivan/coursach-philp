module Modules.File (
  customReadFile,
  customWriteFile
) where
  -- LIBRARIES
  import System.Directory (removeFile, renameFile)

  -- OUTER METHODS
  customReadFile :: String -> IO String
  customReadFile fileName = do
    contents <- readFile fileName
    return contents

  customWriteFile :: String -> String -> String -> IO ()
  customWriteFile name tempName contents = do
    removeFile name
    writeFile tempName contents
    renameFile tempName name