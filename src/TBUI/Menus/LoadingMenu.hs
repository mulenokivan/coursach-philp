module TBUI.Menus.LoadingMenu (
  loadingMenu
) where
  -- LIBRARIES
  import Text.Read (readMaybe)
  import Data.Maybe (fromJust)
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
      "Чтобы создать учебный план, напишите: 'Create <кол-во часов> <id дисциплины> <id семестра> <тип нагрузки>' "
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
    let semesterByLoadingId = head (filter (\s -> getSemesterId s == (read loadingSemesterId :: Integer)) semesterList)

    programContents <- customReadFile _DB_PROGRAM_FILE_NAME
    let programLinesOfFile = lines programContents
    let program = head (filter (\p -> getProgramId p == getSemesterProgramId semesterByLoadingId) (createProgramList programLinesOfFile []))

    let semesterListByProgramId = filter (\s -> getSemesterProgramId s == getProgramId program) semesterList
    let loadingListByProgramId = (\semester -> filter (\l -> getSemesterId semester == getLoadingSemesterId l) loadingList) =<< semesterListByProgramId
    let loadingHoursSumByProgramId = sum (map getLoadingHours loadingListByProgramId)

    let firstError = if (loadingHoursSumByProgramId + read loadingHours :: Integer) >= 240 then "Аяйяяй!" else ""

    let errorMessages = filter (not . null) [firstError]
    if null errorMessages
      then do
        let loadingId = show (getLoadingId (maximumBy (\a b -> compare (getLoadingId a) (getLoadingId b)) loadingList) + 1)
        customWriteFile _DB_LOADING_FILE_NAME _DB_LOADING_TEMP_FILE_NAME (unlines (loadingLinesOfFile ++ ["Loading " ++ loadingId ++ " " ++ loadingHours ++ " " ++ loadingDisciplineId ++ " " ++ loadingSemesterId ++ " \"" ++ loadingKind ++ "\""]))
        putStrLn $ "Нагрузка " ++ loadingId ++ " была создана."
      else do
        mapM_ printError errorMessages
