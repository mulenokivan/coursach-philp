module TBUI.Menus.SemesterMenu (
  semesterMenu
) where
  -- LIBRARIES
  -- LIBRARIES
  import Text.Read (readMaybe, Lexeme (String))
  import Data.Maybe (fromJust)
  import Data.Char (isDigit)
  import Data.List (maximumBy, intercalate)
  import qualified System.Console.ANSI as ANSI

  -- MODELS
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

  semesterMenu :: Int -> IO (String)
  semesterMenu pageNumber = do
    printHeader "Семестры [<id> <номер> <id учебного плана>]"

    contents <- customReadFile _DB_SEMESTER_FILE_NAME
    let linesOfFile = lines contents
    let semesterList = createSemesterList linesOfFile []

    mapM_ (\semester -> do
        printSemester semester
      ) semesterList


    printNoticeList [
      "Чтобы выйти в главное меню, напишите: 'Back'",
      "Чтобы создать семестр, напишите: 'Create <номер> <id учебного плана>' ",
      "Чтобы удалить учебный план, напишите: 'Delete <id>' "
      ]

    input <- getLine
    stringOperations input linesOfFile semesterList 0

  stringOperations :: String -> [String] -> [Semester] -> Int -> IO (String)
  stringOperations inputValue linesOfFile semesterList pageNumber
    | (findSubStrIdx inputValue "Back" 0 /= Nothing) = do
      return "StartMenu"
    | (findSubStrIdx inputValue "Create" 0 /= Nothing) = do
      let [_, numberString, programIdString] = reverseArray (removeQuotesFromArray (splitOnQuotes inputValue [] []))
      createSemester numberString programIdString
      semesterMenu pageNumber
    | (findSubStrIdx inputValue "Delete" 0 /= Nothing) = do
      let [_, idString] = reverseArray (removeQuotesFromArray (splitOnQuotes inputValue [] []))
      let semester = findSemesterById semesterList (read idString :: Integer)
      let semesterNumber = getSemesterNumber semester
      deleteSemester linesOfFile semester
      printSuccess ("Семестр №" ++ "\"" ++ show semesterNumber ++ "\"" ++ " удален")
      semesterMenu pageNumber
    | otherwise = do
      clearScreen
      semesterMenu pageNumber

  createSemester :: String -> String -> IO ()
  createSemester semesterNumber semesterProgramId = do
    semesterContents <- customReadFile _DB_SEMESTER_FILE_NAME
    let semesterLinesOfFile = lines semesterContents
    let semesterList = createSemesterList semesterLinesOfFile []
    let semesterListByProgramId = filter (\s -> getSemesterProgramId s == (read semesterProgramId :: Integer)) semesterList
    let semesterListByProgramIdByNumber = filter (\s -> getSemesterNumber s == (read semesterNumber :: Integer)) semesterListByProgramId

    programContents <- customReadFile _DB_PROGRAM_FILE_NAME
    let programLinesOfFile = lines programContents
    let programList = filter (\p -> getProgramId p == (read semesterProgramId :: Integer)) (createProgramList programLinesOfFile [])

    let firstError = if length semesterListByProgramId >= 8 then "Нельзя создать больше 8 семестров в учебном плане!!!" else ""
    let secondError = if (read semesterNumber :: Integer) > 8 then "Номер семестра не может превосходить 8!!!" else ""
    let thirdError = if not (null semesterListByProgramIdByNumber) then "Нельзя создать два семестра с одинаковым номером в одном учебном плане!!!" else ""
    let fourthError = if null programList then "Нельзя привязать семестр к несуществующему учебному плану" else ""
    let errorMessages = filter (not . null) [firstError, secondError, thirdError, fourthError]
    if null errorMessages
      then do
        let semesterId = show (getSemesterId (maximumBy (\a b -> compare (getSemesterId a) (getSemesterId b)) semesterList) + 1)
        customWriteFile _DB_SEMESTER_FILE_NAME _DB_SEMESTER_TEMP_FILE_NAME (unlines (semesterLinesOfFile ++ ["Semester " ++ semesterId ++ " " ++ semesterNumber ++ " " ++ semesterProgramId]))
        putStrLn $ "Семестр " ++ semesterNumber ++ " был создан."
      else do
        mapM_ printError errorMessages
