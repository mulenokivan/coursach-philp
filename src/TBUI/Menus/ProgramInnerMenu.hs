module TBUI.Menus.ProgramInnerMenu (
  programInnerMenu
) where
  -- LIBRARIES
  import Text.Read (readMaybe)
  import Data.Maybe (fromJust)
  import Data.Char (isDigit)
  import Data.List
  import qualified System.Console.ANSI as ANSI
  -- MODELS
  import Models.Program
  import Models.Discipline
  import Models.Semester
  import Models.Loading


  -- MODULES
  import Modules.File
  import Modules.Search
  import Modules.ReadDB

  -- TBUI
  import TBUI.Tools

  _PAGINATE_PER :: Int
  _PAGINATE_PER = 6

  programInnerMenu :: Integer -> IO (String, Integer)
  programInnerMenu id = do

    programContents <- customReadFile _DB_PROGRAM_FILE_NAME
    let linesOfFile = lines programContents
    let programList = createProgramList linesOfFile []
    let programRecord = findProgramById programList id
    semesterContents <- customReadFile _DB_SEMESTER_FILE_NAME
    let linesOfFile = lines semesterContents
    let wholeSemesterList = createSemesterList linesOfFile []
    let semesterListByProgramId = reverse (findSemestersByProgramId wholeSemesterList id)
    disciplineContents <- customReadFile _DB_DISCIPLINE_FILE_NAME
    let linesOfFile = lines disciplineContents
    let wholeDisciplineList = createDisciplineList linesOfFile []
    let disciplineListByProgramId = findDisciplinesByProgramId wholeDisciplineList id
    loadingContents <- customReadFile _DB_LOADING_FILE_NAME
    let linesOfFile = lines loadingContents
    let wholeLoadingList = createLoadingList linesOfFile []

    printHeader ("Учебный план " ++ "\"" ++ (getProgramTitle programRecord) ++ "\"")

    mapM_ (\semester -> do
        let semesterNumber = getSemesterNumber semester
        putStrLn (show semesterNumber ++ " Семестр")
        let semesterId = getSemesterId semester
        let loadings = findLoadingsBySemesterId wholeLoadingList semesterId
        let disciplineIds = nub (map getLoadingDisciplineId loadings)
        mapM_ (\disciplineId -> do
            let discipline = findDisciplineById disciplineListByProgramId disciplineId
            let disciplineTitle = getDisciplineTitle discipline
            let disciplineLoadingHours = sum (map getLoadingHours (findLoadingsByDisciplineId loadings disciplineId))
            putStrLn ("         " ++ disciplineTitle ++ " " ++ show disciplineLoadingHours ++ " ч.")
          ) disciplineIds
        -- mapM_ (\loading -> do
        --     let loadingId = getLoadingDisciplineId loading
        --     let discipline = findDisciplineById disciplineListByProgramId loadingId
        --     let disciplineTitle = getDisciplineTitle discipline
        --     let loadingHours = getLoadingHours loading
        --     let loadingKind = getLoadingKind loading
        --     putStrLn ("         " ++ disciplineTitle ++ " " ++ show loadingHours ++ " " ++ loadingKind)
        --   ) loadings
      ) semesterListByProgramId

    printNoticeList [
      "Чтобы вернуться в главное меню, введите: 'Back'"
      ]

    input <- getLine
    stringOperations input linesOfFile programList id

  stringOperations :: String -> [String] -> [Program] -> Integer -> IO (String, Integer)
  stringOperations inputValue linesOfFile programList id
    | (findSubStrIdx inputValue "Back" 0 /= Nothing) = do
      return ("StartMenu", 1)
    | otherwise = do
      clearScreen
      programInnerMenu id

  createProgram :: String -> String -> IO ()
  createProgram programTitle programSpecialityId = do
    contents <- customReadFile _DB_PROGRAM_FILE_NAME
    let linesOfFile = lines contents
    let programList = createProgramList linesOfFile []
    let programId = show (getProgramId (maximumBy (\a b -> compare (getProgramId a) (getProgramId b)) programList) + 1)
    customWriteFile _DB_PROGRAM_FILE_NAME _DB_PROGRAM_TEMP_FILE_NAME (unlines (linesOfFile ++ ["Program " ++ programId ++ " \"" ++ programTitle ++ "\" " ++ programSpecialityId]))
