module Models.Program (
  Program,
  _DB_PROGRAM_FILE_NAME,
  _DB_PROGRAM_TEMP_FILE_NAME,
  createLocalProgram,
  deleteProgram,
  createProgramList,
  getProgramId,
  getProgramTitle,
  getProgramSpecialityId,
  ifProgramFileExist,
  printProgram
) where
  -- LIBRARIES
  import System.Directory (doesFileExist)
  import Data.List (intersperse)

  -- MODULES
  import Modules.ReadDB
  import Modules.File (customWriteFile)

  -- TYPES
  data Program = Program {
    id :: Integer,
    title :: String,
    speciality_id :: Integer
  }

  instance Show Program where
    show (Program id title speciality_id) =
      "Program " ++ show title ++ " " ++ show speciality_id

  -- CONSTANTS
  _DB_PROGRAM_FILE_NAME :: String
  _DB_PROGRAM_FILE_NAME = "db/program_db.txt"

  _DB_PROGRAM_TEMP_FILE_NAME :: String
  _DB_PROGRAM_TEMP_FILE_NAME = "db/program_db_temp.txt"

  -- CONSTRUCTORS
  createLocalProgram :: Integer -> String -> Integer -> Program
  createLocalProgram id title specialityId = Program id title specialityId

  -- OPERATIONS WITH RECORDS
  deleteProgram :: [String] -> Program -> IO ()
  deleteProgram linesOfFile program = do
    let filteredLinesOfFile = filter (/= show program) linesOfFile
    let test = concat $ intersperse "\n" filteredLinesOfFile
    customWriteFile _DB_PROGRAM_FILE_NAME _DB_PROGRAM_TEMP_FILE_NAME test

  createProgramList :: [String] -> [Program] -> [Program]
  createProgramList [] answer = answer
  createProgramList (x:xs) answer =
    createProgramList xs (record:answer)

    where
      [_, idString, titleString, specialityIdString] = reverseArray (removeQuotesFromArray (splitOnQuotes x [] []))
      id = read idString :: Integer
      specialityId = read specialityIdString :: Integer
      record = Program id titleString specialityId

  -- PRINT
  printProgram :: Program -> IO ()
  printProgram (Program id title specialityId) = putStrLn (title ++ " " ++ show specialityId)

  -- GETTERS
  getProgramId :: Program -> Integer
  getProgramId (Program id _ _) = id

  getProgramTitle :: Program -> String
  getProgramTitle (Program _ title _) = title

  getProgramSpecialityId :: Program -> Integer
  getProgramSpecialityId (Program _ _ speciality_id) = speciality_id

  -- CONDITIONS
  ifProgramFileExist :: IO ()
  ifProgramFileExist = do
    exists <- doesFileExist _DB_PROGRAM_FILE_NAME
    if exists then
      return ()
    else
      writeFile _DB_PROGRAM_FILE_NAME ""