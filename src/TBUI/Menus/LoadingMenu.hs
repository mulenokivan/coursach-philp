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
    printHeader "Нагрузки"

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
      return "StartMenu"
    | otherwise = do
      clearScreen
      loadingMenu

  createLoading :: String -> String -> String -> String -> IO ()
  createLoading loadingHours loadingDisciplineId loadingSemesterId loadingKind = do
    contents <- customReadFile _DB_LOADING_FILE_NAME
    let linesOfFile = lines contents
    let loadingList = createLoadingList linesOfFile []
    let loadingId = show (getLoadingId (maximumBy (\a b -> compare (getLoadingId a) (getLoadingId b)) loadingList) + 1)
    customWriteFile _DB_LOADING_FILE_NAME _DB_LOADING_TEMP_FILE_NAME (unlines (linesOfFile ++ ["Loading " ++ loadingId ++ " " ++ loadingHours ++ " " ++ loadingDisciplineId ++ " " ++ loadingSemesterId ++ " \"" ++ loadingKind ++ "\""]))
