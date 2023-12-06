module TBUI.Menus.SemesterMenu (
  semesterMenu
) where
  -- LIBRARIES
  import Text.Read (readMaybe)
  import Data.Maybe (fromJust)
  import Data.Char (isDigit)
  import Data.List (maximumBy, intercalate)
  import qualified System.Console.ANSI as ANSI

  -- MODELS
  import Models.Semester

  -- MODULES
  import Modules.File
  import Modules.Search
  import Modules.ReadDB

  -- TBUI
  import TBUI.Tools

  _PAGINATE_PER :: Int
  _PAGINATE_PER = 6

  semesterMenu :: IO (String)
  semesterMenu = do
    printHeader "Направления подготовки"

    contents <- customReadFile _DB_SEMESTER_FILE_NAME
    let linesOfFile = lines contents
    let semesterList = createSemesterList linesOfFile []

    mapM_ (\semester -> do
        printSemester semester
      ) semesterList

    printNoticeList [
      "Чтобы выйти в главное меню, напишите: 'Back'",
      "Чтобы создать направление подготовки, напишите: 'Create <номер> <id учебного плана>' "
      ]

    input <- getLine
    stringOperations input linesOfFile semesterList

  stringOperations :: String -> [String] -> [Semester] -> IO (String)
  stringOperations inputValue linesOfFile semesterList
    | (findSubStrIdx inputValue "Back" 0 /= Nothing) = do
      return "StartMenu"
    | (findSubStrIdx inputValue "Create" 0 /= Nothing) = do
      let [_, titleString, specialityIdString] = reverseArray (removeQuotesFromArray (splitOnQuotes inputValue [] []))
      createSemester titleString specialityIdString
      return "StartMenu"
    | otherwise = do
      clearScreen
      semesterMenu

  createSemester :: String -> String -> IO ()
  createSemester semesterTitle semesterSpecialityId = do
    contents <- customReadFile _DB_SEMESTER_FILE_NAME
    let linesOfFile = lines contents
    let semesterList = createSemesterList linesOfFile []
    let semesterId = show (getSemesterId (maximumBy (\a b -> compare (getSemesterId a) (getSemesterId b)) semesterList) + 1)
    customWriteFile _DB_SEMESTER_FILE_NAME _DB_SEMESTER_TEMP_FILE_NAME (unlines (linesOfFile ++ ["Semester " ++ semesterId ++ " " ++ semesterTitle ++ " " ++ semesterSpecialityId]))