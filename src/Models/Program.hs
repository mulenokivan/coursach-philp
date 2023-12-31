{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use isNothing" #-}
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
  updateProgram,
  printProgram
) where
  -- LIBRARIES
  import System.Directory (doesFileExist)
  import Data.List (intersperse)

  -- MODULES
  import Modules.ReadDB
  import Modules.File
  import Models.Semester
  import Models.Discipline

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
  deleteProgram :: Program -> IO ()
  deleteProgram program = do
    programContents <- customReadFile _DB_PROGRAM_FILE_NAME
    let programlinesOfFile = lines programContents
    let programId = show (getProgramId program)
    let filteredLinesOfFile = filter (\line -> findLineById line (read programId :: Integer)) programlinesOfFile
    let test = concat $ intersperse "\n" filteredLinesOfFile
    semesterContents <- customReadFile _DB_SEMESTER_FILE_NAME
    let semesterlinesOfFile = lines semesterContents
    let semesterList = findSemestersByProgramId (createSemesterList semesterlinesOfFile []) (read programId :: Integer)
    mapM_(\semester -> do
        deleteSemester semester
      ) semesterList
    disciplineContents <- customReadFile _DB_DISCIPLINE_FILE_NAME
    let disciplinelinesOfFile = lines disciplineContents
    let disciplineList = findDisciplinesByProgramId (createDisciplineList disciplinelinesOfFile []) (read programId :: Integer)
    mapM_(\discipline -> do
        deleteDiscipline discipline
      ) disciplineList
    customWriteFile _DB_PROGRAM_FILE_NAME _DB_PROGRAM_TEMP_FILE_NAME test

  updateProgram :: Program -> IO ()
  updateProgram program = do
    programContents <- customReadFile _DB_PROGRAM_FILE_NAME
    let programlinesOfFile = lines programContents
    let programId = show (getProgramId program)
    let programTitle = getProgramTitle program
    let programSpecialityId = show (getProgramSpecialityId program)
    let filteredLinesOfFile = filter (\line -> findLineById line (read programId :: Integer)) programlinesOfFile
    let test = concat $ intersperse "\n" filteredLinesOfFile
    customWriteFile _DB_PROGRAM_FILE_NAME _DB_PROGRAM_TEMP_FILE_NAME (test ++ "\n" ++ "Program " ++ programId ++ " \"" ++ programTitle ++ "\" " ++ programSpecialityId)

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
  printProgram (Program id title specialityId) = putStrLn (show id ++  " " ++ title ++ " " ++ show specialityId)

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

  -- Other
  findSubStrIdx :: String -> String -> Integer -> Maybe Integer
  findSubStrIdx "" _ _ = Nothing
  findSubStrIdx s target n
    | take (length target) s == target = Just n
    | otherwise = findSubStrIdx (tail s) target (n + 1)

  findSemestersByProgramId :: [Semester] -> Integer -> [Semester]
  findSemestersByProgramId [] _ = []
  findSemestersByProgramId (x:xs) programId
    | (getSemesterProgramId x == programId) = x : findSemestersByProgramId xs programId
    | otherwise = findSemestersByProgramId xs programId

  findDisciplinesByProgramId :: [Discipline] -> Integer -> [Discipline]
  findDisciplinesByProgramId [] _ = []
  findDisciplinesByProgramId (x:xs) programId
    | (getDisciplineProgramId x == programId) = x : findDisciplinesByProgramId xs programId
    | otherwise = findDisciplinesByProgramId xs programId