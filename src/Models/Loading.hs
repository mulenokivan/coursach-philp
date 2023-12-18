module Models.Loading (
  Loading,
  _DB_LOADING_FILE_NAME,
  _DB_LOADING_TEMP_FILE_NAME,
  createLocalLoading,
  deleteLoading,
  createLoadingList,
  getLoadingId,
  getLoadingHours,
  getLoadingDisciplineId,
  getLoadingSemesterId,
  getLoadingKind,
  getLoadingKindAndHours,
  getLoadingSemesterIdAndDisciplineId,
  ifLoadingFileExist,
  printLoading
) where
  -- LIBRARIES
  import System.Directory (doesFileExist)
  import Data.List (intersperse)

  -- MODULES
  import Modules.ReadDB
  import Modules.File (customWriteFile)

  -- TYPES
  data Loading = Loading {
    id :: Integer,
    hours :: Integer,
    discipline_id :: Integer,
    semester_id :: Integer,
    kind :: String
  }

  instance Show Loading where
    show (Loading id hours discipline_id semester_id kind) =
      "Loading " ++ show id ++ " " ++ show hours ++ " " ++ show discipline_id ++ " " ++ show semester_id ++ " " ++ show kind

  -- CONSTANTS
  _DB_LOADING_FILE_NAME :: String
  _DB_LOADING_FILE_NAME = "db/loading_db.txt"

  _DB_LOADING_TEMP_FILE_NAME :: String
  _DB_LOADING_TEMP_FILE_NAME = "db/loading_db_temp.txt"

  -- CONSTRUCTORS
  createLocalLoading :: Integer -> Integer -> Integer -> Integer -> String -> Loading
  createLocalLoading id hours discipline_id semester_id kind = Loading id hours discipline_id semester_id kind

  -- OPERATIONS WITH RECORDS
  deleteLoading :: [String] -> Loading -> IO ()
  deleteLoading linesOfFile loading = do
    let filteredLinesOfFile = filter (/= show loading) linesOfFile
    let test = concat $ intersperse "\n" filteredLinesOfFile
    customWriteFile _DB_LOADING_FILE_NAME _DB_LOADING_TEMP_FILE_NAME test

  createLoadingList :: [String] -> [Loading] -> [Loading]
  createLoadingList [] answer = answer
  createLoadingList (x:xs) answer =
    createLoadingList xs (record:answer)

    where
      [_, idString, hoursString, disciplineIdString, semesterIdString, kindString] = reverseArray (removeQuotesFromArray (splitOnQuotes x [] []))
      id = read idString :: Integer
      hours = read hoursString :: Integer
      disciplineId = read disciplineIdString :: Integer
      semesterId = read semesterIdString :: Integer
      record = Loading id hours disciplineId semesterId kindString

  -- PRINT
  printLoading :: Loading -> IO ()
  printLoading (Loading id hours disciplineId semesterId kindString) = putStrLn (show id ++ " " ++ show hours ++ " " ++ show disciplineId ++ " " ++ show semesterId ++ " " ++ kindString )

  -- GETTERS
  getLoadingId :: Loading -> Integer
  getLoadingId (Loading id _ _ _ _) = id

  getLoadingHours :: Loading -> Integer
  getLoadingHours (Loading _ hours _ _ _) = hours

  getLoadingDisciplineId :: Loading -> Integer
  getLoadingDisciplineId (Loading _ _ discipline_id _ _) = discipline_id

  getLoadingSemesterId :: Loading -> Integer
  getLoadingSemesterId (Loading _ _ _ semester_id _) = semester_id

  getLoadingSemesterIdAndDisciplineId :: Loading -> [Integer]
  getLoadingSemesterIdAndDisciplineId (Loading _ _ discipline_id semester_id _) = [discipline_id, semester_id]

  getLoadingKind :: Loading -> String
  getLoadingKind (Loading _ _ _ _ kind) = kind

  getLoadingKindAndHours :: Loading -> [String]
  getLoadingKindAndHours (Loading _ hours _ _ kind) = [show hours, kind]

  -- CONDITIONS
  ifLoadingFileExist :: IO ()
  ifLoadingFileExist = do
    exists <- doesFileExist _DB_LOADING_FILE_NAME
    if exists then
      return ()
    else
      writeFile _DB_LOADING_FILE_NAME ""