module Models.Semester (
  Semester,
  _DB_SEMESTER_FILE_NAME,
  _DB_SEMESTER_TEMP_FILE_NAME,
  createLocalSemester,
  deleteSemester,
  createSemesterList,
  getSemesterId,
  getSemesterNumber,
  getSemesterProgramId,
  ifSemesterFileExist,
  printSemester
) where
  -- LIBRARIES
  import System.Directory (doesFileExist)
  import Data.List (intersperse)

  -- MODULES
  import Modules.ReadDB
  import Modules.File (customWriteFile)

  -- TYPES
  data Semester = Semester {
    id :: Integer,
    number :: Integer,
    program_id :: Integer
  }

  instance Show Semester where
    show (Semester id number program_id) =
      "Semester " ++ show id ++ " " ++ show number ++ " " ++ show program_id

  -- CONSTANTS
  _DB_SEMESTER_FILE_NAME :: String
  _DB_SEMESTER_FILE_NAME = "db/semester_db.txt"

  _DB_SEMESTER_TEMP_FILE_NAME :: String
  _DB_SEMESTER_TEMP_FILE_NAME = "db/semester_db_temp.txt"

  -- CONSTRUCTORS
  createLocalSemester :: Integer -> Integer -> Integer -> Semester
  createLocalSemester id number programId = Semester id number programId

  -- OPERATIONS WITH RECORDS
  deleteSemester :: [String] -> Semester -> IO ()
  deleteSemester linesOfFile semester = do
    let semesterId = show (getSemesterId semester)
    let filteredLinesOfFile = filter (\line -> findSubStrIdx line ("Semester " ++ semesterId) 0 == Nothing) linesOfFile
    let test = concat $ intersperse "\n" filteredLinesOfFile
    customWriteFile _DB_SEMESTER_FILE_NAME _DB_SEMESTER_TEMP_FILE_NAME test

  createSemesterList :: [String] -> [Semester] -> [Semester]
  createSemesterList [] answer = answer
  createSemesterList (x:xs) answer =
    createSemesterList xs (record:answer)

    where
      [_, idString, numberString, programIdString] = reverseArray (removeQuotesFromArray (splitOnQuotes x [] []))
      id = read idString :: Integer
      programId = read programIdString :: Integer
      number = read numberString :: Integer
      record = Semester id number programId

  -- PRINT
  printSemester :: Semester -> IO ()
  printSemester (Semester id number programId) = putStrLn (show id ++ " " ++ show number ++ " " ++ show programId)

  -- GETTERS
  getSemesterId :: Semester -> Integer
  getSemesterId (Semester id _ _) = id

  getSemesterNumber :: Semester -> Integer
  getSemesterNumber (Semester _ number _) = number

  getSemesterProgramId :: Semester -> Integer
  getSemesterProgramId (Semester _ _ program_id) = program_id

  -- CONDITIONS
  ifSemesterFileExist :: IO ()
  ifSemesterFileExist = do
    exists <- doesFileExist _DB_SEMESTER_FILE_NAME
    if exists then
      return ()
    else
      writeFile _DB_SEMESTER_FILE_NAME ""

  -- Other
  findSubStrIdx :: String -> String -> Integer -> Maybe Integer
  findSubStrIdx "" _ _ = Nothing
  findSubStrIdx s target n
    | take (length target) s == target = Just n
    | otherwise = findSubStrIdx (tail s) target (n + 1)