module Models.Discipline (
  Discipline,
  _DB_DISCIPLINE_FILE_NAME,
  _DB_DISCIPLINE_TEMP_FILE_NAME,
  createLocalDiscipline,
  deleteDiscipline,
  createDisciplineList,
  getDisciplineId,
  getDisciplineTitle,
  getDisciplineProgramId,
  ifDisciplineFileExist,
  printDiscipline
) where
  -- LIBRARIES
  import System.Directory (doesFileExist)
  import Data.List (intersperse)

  -- MODULES
  import Modules.ReadDB
  import Modules.File (customWriteFile)

  -- TYPES
  data Discipline = Discipline {
    id :: Integer,
    title :: String,
    program_id :: Integer
  }

  instance Show Discipline where
    show (Discipline id title program_id) =
      "Discipline " ++ show id ++ " " ++ show title ++ " " ++ show program_id

  -- CONSTANTS
  _DB_DISCIPLINE_FILE_NAME :: String
  _DB_DISCIPLINE_FILE_NAME = "db/discipline_db.txt"

  _DB_DISCIPLINE_TEMP_FILE_NAME :: String
  _DB_DISCIPLINE_TEMP_FILE_NAME = "db/discipline_db_temp.txt"

  -- CONSTRUCTORS
  createLocalDiscipline :: Integer -> String -> Integer -> Discipline
  createLocalDiscipline id title programId = Discipline id title programId

  -- OPERATIONS WITH RECORDS
  deleteDiscipline :: [String] -> Discipline -> IO ()
  deleteDiscipline linesOfFile discipline = do
    let disciplineId = show (getDisciplineId discipline)
    let filteredLinesOfFile = filter (\line -> findSubStrIdx line ("Discipline " ++ disciplineId) 0 == Nothing) linesOfFile
    let test = concat $ intersperse "\n" filteredLinesOfFile
    customWriteFile _DB_DISCIPLINE_FILE_NAME _DB_DISCIPLINE_TEMP_FILE_NAME test

  createDisciplineList :: [String] -> [Discipline] -> [Discipline]
  createDisciplineList [] answer = answer
  createDisciplineList (x:xs) answer =
    createDisciplineList xs (record:answer)

    where
      [_, idString, titleString, programIdString] = reverseArray (removeQuotesFromArray (splitOnQuotes x [] []))
      id = read idString :: Integer
      programId = read programIdString :: Integer
      record = Discipline id titleString programId

  -- PRINT
  printDiscipline :: Discipline -> IO ()
  printDiscipline (Discipline id title programId) = putStrLn (show id ++ " " ++ title ++ " " ++ show programId)

  -- GETTERS
  getDisciplineId :: Discipline -> Integer
  getDisciplineId (Discipline id _ _) = id

  getDisciplineTitle :: Discipline -> String
  getDisciplineTitle (Discipline _ title _) = title

  getDisciplineProgramId :: Discipline -> Integer
  getDisciplineProgramId (Discipline _ _ program_id) = program_id

  -- CONDITIONS
  ifDisciplineFileExist :: IO ()
  ifDisciplineFileExist = do
    exists <- doesFileExist _DB_DISCIPLINE_FILE_NAME
    if exists then
      return ()
    else
      writeFile _DB_DISCIPLINE_FILE_NAME ""

  -- Other
  findSubStrIdx :: String -> String -> Integer -> Maybe Integer
  findSubStrIdx "" _ _ = Nothing
  findSubStrIdx s target n
    | take (length target) s == target = Just n
    | otherwise = findSubStrIdx (tail s) target (n + 1)