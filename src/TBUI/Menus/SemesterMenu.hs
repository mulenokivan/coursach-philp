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

  semesterMenu :: Int -> IO (String)
  semesterMenu pageNumber = do
    printHeader "Семестры"

    contents <- customReadFile _DB_SEMESTER_FILE_NAME
    let linesOfFile = lines contents
    let semesterList = createSemesterList linesOfFile []

    let filteredSemesterList = take _PAGINATE_PER (drop ((pageNumber - 1) * _PAGINATE_PER) semesterList)

    mapM_ (\semester -> do
        printSemester semester
      ) filteredSemesterList

    printNoticeList [
      "Чтобы выйти в главное меню, напишите: 'Back'",
      "Чтобы создать семестр, напишите: 'Create <номер> <id учебного плана>' "
      ]

    let pageCount = ceiling (fromIntegral (length semesterList) / fromIntegral _PAGINATE_PER)
    printPagination pageNumber pageCount

    input <- getLine
    if (all isDigit input)
      then do
        let index = read input :: Int
        digitOperations index pageCount pageNumber
      else do
        stringOperations input linesOfFile semesterList pageNumber

  digitOperations :: Int -> Int -> Int -> IO (String)
  digitOperations index pageCount pageNumber
    | (index <= pageCount) = do
      clearScreen
      semesterMenu index
    | otherwise = do
      clearScreen
      printError "Выход за пределы пагинации"
      semesterMenu pageNumber

  stringOperations :: String -> [String] -> [Semester] -> Int -> IO (String)
  stringOperations inputValue linesOfFile semesterList pageNumber
    | (findSubStrIdx inputValue "Back" 0 /= Nothing) = do
      return "StartMenu"
    | (findSubStrIdx inputValue "Create" 0 /= Nothing) = do
      let [_, titleString, specialityIdString] = reverseArray (removeQuotesFromArray (splitOnQuotes inputValue [] []))
      createSemester titleString specialityIdString
      return "StartMenu"
    | otherwise = do
      clearScreen
      semesterMenu pageNumber

  createSemester :: String -> String -> IO ()
  createSemester semesterTitle semesterSpecialityId = do
    contents <- customReadFile _DB_SEMESTER_FILE_NAME
    let linesOfFile = lines contents
    let semesterList = createSemesterList linesOfFile []
    let semesterId = show (getSemesterId (maximumBy (\a b -> compare (getSemesterId a) (getSemesterId b)) semesterList) + 1)
    customWriteFile _DB_SEMESTER_FILE_NAME _DB_SEMESTER_TEMP_FILE_NAME (unlines (linesOfFile ++ ["Semester " ++ semesterId ++ " " ++ semesterTitle ++ " " ++ semesterSpecialityId]))
