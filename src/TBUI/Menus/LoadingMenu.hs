module TBUI.Menus.LoadingMenu (
  loadingMenu
) where
  -- LIBRARIES
  import Text.Read (readMaybe)
  import Data.Maybe (fromJust, isNothing)
  import Data.Char (isDigit)
  import Data.List (maximumBy, intercalate)
  import qualified System.Console.ANSI as ANSI

  -- MODELS
  import Models.Loading
  import Models.Semester
  import Models.Program

  -- MODULES
  import Modules.File
  import Modules.Search
  import Modules.ReadDB

  -- TBUI
  import TBUI.Tools
  import Control.Monad (guard)

  _PAGINATE_PER :: Int
  _PAGINATE_PER = 6

  loadingMenu :: IO (String)
  loadingMenu = do
    printHeader "Нагрузки [<id> <часы> <id дисциплины> <id семестра> <тип>]"

    contents <- customReadFile _DB_LOADING_FILE_NAME
    let linesOfFile = lines contents
    let loadingList = createLoadingList linesOfFile []

    mapM_ (\loading -> do
        printLoading loading
      ) loadingList

    printNoticeList [
      "Чтобы выйти в главное меню, напишите: 'Back'",
      "Чтобы создать учебный план, напишите: 'Create <кол-во часов> <id дисциплины> <id семестра> <тип нагрузки>' ",
      "Чтобы удалить нагрузку, напишите: 'Delete <id>' "
      ]

    input <- getLine
    stringOperations input linesOfFile loadingList

  stringOperations :: String -> [String] -> [Loading] -> IO (String)
  stringOperations inputValue linesOfFile loadingList
    | (findSubStrIdx inputValue "Back" 0 /= Nothing) = do
      return "StartMenu"
    | (findSubStrIdx inputValue "Create" 0 /= Nothing) = do
      let [_, hoursString, disciplineIdString, semesterIdString, kindString] = reverseArray (removeQuotesFromArray (splitOnQuotes inputValue [] []))
      createLoading hoursString disciplineIdString semesterIdString kindString
      loadingMenu
    | (findSubStrIdx inputValue "Delete" 0 /= Nothing) = do
      let [_, idString] = reverseArray (removeQuotesFromArray (splitOnQuotes inputValue [] []))
      let loading = findLoadingById loadingList (read idString :: Integer)
      deleteLoading linesOfFile loading
      printSuccess "Нагрузка удалена"
      loadingMenu
    | otherwise = do
      clearScreen
      loadingMenu

  createLoading :: String -> String -> String -> String -> IO ()
  createLoading loadingHours loadingDisciplineId loadingSemesterId loadingKind = do

    loadingContents <- customReadFile _DB_LOADING_FILE_NAME
    let loadingLinesOfFile = lines loadingContents
    let loadingList = createLoadingList loadingLinesOfFile []

    semesterContents <- customReadFile _DB_SEMESTER_FILE_NAME
    let semesterLinesOfFile = lines semesterContents
    let semesterList = createSemesterList semesterLinesOfFile []
    let semestersByLoadingId = filter (\s -> getSemesterId s == (read loadingSemesterId :: Integer)) semesterList
    let semesterByLoadingId = head semestersByLoadingId

    programContents <- customReadFile _DB_PROGRAM_FILE_NAME
    let programLinesOfFile = lines programContents
    let program = head (filter (\p -> getProgramId p == getSemesterProgramId semesterByLoadingId) (createProgramList programLinesOfFile []))

    let semesterListByProgramId = filter (\s -> getSemesterProgramId s == getProgramId program) semesterList
    let loadingListByProgramId = (\semester -> filter (\l -> getSemesterId semester == getLoadingSemesterId l) loadingList) =<< semesterListByProgramId
    let loadingHoursSumByProgramId = sum (map getLoadingHours loadingListByProgramId)

    let semesterListByFirstYear = filter (\s -> getSemesterNumber s == 1) semesterListByProgramId ++ filter (\s -> getSemesterNumber s == 2) semesterListByProgramId
    let semesterListBySecondYear = filter (\s -> getSemesterNumber s == 3) semesterListByProgramId ++ filter (\s -> getSemesterNumber s == 4) semesterListByProgramId
    let semesterListByThirdYear = filter (\s -> getSemesterNumber s == 5) semesterListByProgramId ++ filter (\s -> getSemesterNumber s == 6) semesterListByProgramId
    let semesterListByFourthYear = filter (\s -> getSemesterNumber s == 7) semesterListByProgramId ++ filter (\s -> getSemesterNumber s == 8) semesterListByProgramId

    let loadingListByFirstYear = (\semester -> filter (\l -> getSemesterId semester == getLoadingSemesterId l) loadingList) =<< semesterListByFirstYear
    let loadingListBySecondYear = (\semester -> filter (\l -> getSemesterId semester == getLoadingSemesterId l) loadingList) =<< semesterListBySecondYear
    let loadingListByThirdYear = (\semester -> filter (\l -> getSemesterId semester == getLoadingSemesterId l) loadingList) =<< semesterListByThirdYear
    let loadingListByFourthYear = (\semester -> filter (\l -> getSemesterId semester == getLoadingSemesterId l) loadingList) =<< semesterListByFourthYear

    let loadingHoursSumByFirstYear = sum (map getLoadingHours loadingListByFirstYear)
    let loadingHoursSumBySecondYear = sum (map getLoadingHours loadingListBySecondYear)
    let loadingHoursSumByThirdYear = sum (map getLoadingHours loadingListByThirdYear)
    let loadingHoursSumByFourthYear = sum (map getLoadingHours loadingListByFourthYear)

    let semesterNumber = getSemesterNumber semesterByLoadingId

    let firstError = if (loadingHoursSumByProgramId + read loadingHours :: Integer) > 8640 then "Нельзя, чтобы суммарная нагрузка в учебном плане превышала 8640 часов. Текущая нагрузка: " ++ show loadingHoursSumByProgramId else ""
    let secondError = if (semesterNumber == 1 || semesterNumber == 2) && (loadingHoursSumByFirstYear + read loadingHours :: Integer) > 2520 then "Нельзя, чтобы суммарная нагрузка в учебном плане за год превышала 2520 часов. Текущая нагрузка в выбранном учебном году: " ++ show loadingHoursSumByFirstYear else ""
    let thirdError = if (semesterNumber == 3 || semesterNumber == 4) && (loadingHoursSumBySecondYear + read loadingHours :: Integer) > 2520 then "Нельзя, чтобы суммарная нагрузка в учебном плане за год превышала 2520 часов. Текущая нагрузка в выбранном учебном году: " ++ show loadingHoursSumBySecondYear else ""
    let fourthError = if (semesterNumber == 5 || semesterNumber == 6) && (loadingHoursSumByThirdYear + read loadingHours :: Integer) > 2520 then "Нельзя, чтобы суммарная нагрузка в учебном плане за год превышала 2520 часов. Текущая нагрузка в выбранном учебном году: " ++ show loadingHoursSumByThirdYear else ""
    let fifthError = if (semesterNumber == 7 || semesterNumber == 8) && (loadingHoursSumByFourthYear + read loadingHours :: Integer) > 2520 then "Нельзя, чтобы суммарная нагрузка в учебном плане за год превышала 2520 часов. Текущая нагрузка в выбранном учебном году: " ++ show loadingHoursSumByFourthYear else ""

    let errorMessages = filter (not . null) [firstError, secondError, thirdError, fourthError, fifthError]
    if null errorMessages
      then do
        let loadingId = show (getLoadingId (maximumBy (\a b -> compare (getLoadingId a) (getLoadingId b)) loadingList) + 1)
        customWriteFile _DB_LOADING_FILE_NAME _DB_LOADING_TEMP_FILE_NAME (unlines (loadingLinesOfFile ++ ["Loading " ++ loadingId ++ " " ++ loadingHours ++ " " ++ loadingDisciplineId ++ " " ++ loadingSemesterId ++ " \"" ++ loadingKind ++ "\""]))
        putStrLn $ "Нагрузка " ++ loadingId ++ " была создана."
      else do
        mapM_ printError errorMessages
