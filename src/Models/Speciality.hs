module Models.Speciality (
  Speciality,
  _DB_SPECIALITY_FILE_NAME,
  _DB_SPECIALITY_TEMP_FILE_NAME,
  createLocalSpeciality,
  deleteSpeciality,
  createSpecialityList,
  getSpecialityId,
  getSpecialityTitle,
  getSpecialityCode,
  ifSpecialityFileExist,
  printSpeciality
) where
  -- LIBRARIES
  import System.Directory (doesFileExist)
  import Data.List (intersperse)

  -- MODULES
  import Modules.ReadDB
  import Modules.File (customWriteFile)

  -- TYPES
  data Speciality = Speciality {
    id :: Integer,
    title :: String,
    code :: String
  }

  instance Show Speciality where
    show (Speciality id title code) =
      "Speciality " ++ show id ++ " " ++ show title ++ " " ++ show code

  -- CONSTANTS
  _DB_SPECIALITY_FILE_NAME :: String
  _DB_SPECIALITY_FILE_NAME = "db/speciality_db.txt"

  _DB_SPECIALITY_TEMP_FILE_NAME :: String
  _DB_SPECIALITY_TEMP_FILE_NAME = "db/speciality_db_temp.txt"

  -- CONSTRUCTORS
  createLocalSpeciality :: Integer -> String -> String -> Speciality
  createLocalSpeciality id title code = Speciality id title code

  -- OPERATIONS WITH RECORDS
  deleteSpeciality :: [String] -> Speciality -> IO ()
  deleteSpeciality linesOfFile speciality = do
    let filteredLinesOfFile = filter (/= show speciality) linesOfFile
    let test = concat $ intersperse "\n" filteredLinesOfFile
    customWriteFile _DB_SPECIALITY_FILE_NAME _DB_SPECIALITY_TEMP_FILE_NAME test

  createSpecialityList :: [String] -> [Speciality] -> [Speciality]
  createSpecialityList [] answer = answer
  createSpecialityList (x:xs) answer =
    createSpecialityList xs (record:answer)

    where
      [_, idString, titleString, codeString] = reverseArray (removeQuotesFromArray (splitOnQuotes x [] []))
      id = read idString :: Integer
      record = Speciality id titleString codeString

  -- PRINT
  printSpeciality :: Speciality -> IO ()
  printSpeciality (Speciality id title code) = putStrLn (show id ++ " " ++ title ++ " " ++ code)

  -- GETTERS
  getSpecialityId :: Speciality -> Integer
  getSpecialityId (Speciality id _ _) = id

  getSpecialityTitle :: Speciality -> String
  getSpecialityTitle (Speciality _ title _) = title

  getSpecialityCode :: Speciality -> String
  getSpecialityCode (Speciality _ _ code) = code

  -- CONDITIONS
  ifSpecialityFileExist :: IO ()
  ifSpecialityFileExist = do
    exists <- doesFileExist _DB_SPECIALITY_FILE_NAME
    if exists then
      return ()
    else
      writeFile _DB_SPECIALITY_FILE_NAME ""