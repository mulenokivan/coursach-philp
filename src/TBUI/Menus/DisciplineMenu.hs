module TBUI.Menus.DisciplineMenu (
  disciplineMenu
) where
  -- LIBRARIES
  import Text.Read (readMaybe)
  import Data.Maybe (fromJust)
  import Data.Char (isDigit)
  import Data.List (maximumBy, intercalate)
  import qualified System.Console.ANSI as ANSI

  -- MODELS
  import Models.Discipline
  import Models.Program

  -- MODULES
  import Modules.File
  import Modules.Search
  import Modules.ReadDB

  -- TBUI
  import TBUI.Tools

  _PAGINATE_PER :: Int
  _PAGINATE_PER = 6

  disciplineMenu :: IO (String)
  disciplineMenu = do
    printHeader "Дисциплины"

    contents <- customReadFile _DB_DISCIPLINE_FILE_NAME
    let linesOfFile = lines contents
    let disciplineList = createDisciplineList linesOfFile []

    mapM_ (\discipline -> do
        printDiscipline discipline
      ) disciplineList

    printNoticeList [
      "Чтобы выйти в главное меню, напишите: 'Back'",
      "Чтобы создать учебный план, напишите: 'Create <название> <id учебного плана>' ",
      "Чтобы удалить дисциплину, напишите: 'Delete <id>' "
      ]

    input <- getLine
    stringOperations input linesOfFile disciplineList

  stringOperations :: String -> [String] -> [Discipline] -> IO (String)
  stringOperations inputValue linesOfFile disciplineList
    | (findSubStrIdx inputValue "Back" 0 /= Nothing) = do
      return "StartMenu"
    | (findSubStrIdx inputValue "Create" 0 /= Nothing) = do
      let [_, titleString, programIdString] = reverseArray (removeQuotesFromArray (splitOnQuotes inputValue [] []))
      createDiscipline titleString programIdString
      disciplineMenu
    | (findSubStrIdx inputValue "Delete" 0 /= Nothing) = do
      let [_, idString] = reverseArray (removeQuotesFromArray (splitOnQuotes inputValue [] []))
      let discipline = findDisciplineById disciplineList (read idString :: Integer)
      let disciplineTitle = getDisciplineTitle discipline
      deleteDiscipline linesOfFile discipline
      printSuccess ("Дисциплина " ++ "\"" ++ disciplineTitle ++ "\"" ++ " удалена")
      disciplineMenu
    | otherwise = do
      clearScreen
      disciplineMenu

  createDiscipline :: String -> String -> IO ()
  createDiscipline disciplineTitle disciplineProgramId = do
    disciplineContents <- customReadFile _DB_DISCIPLINE_FILE_NAME
    let disciplineLinesOfFile = lines disciplineContents
    let disciplineList = createDisciplineList disciplineLinesOfFile []

    programContents <- customReadFile _DB_PROGRAM_FILE_NAME
    let programLinesOfFile = lines programContents
    let programList = filter (\p -> getProgramId p == (read disciplineProgramId :: Integer)) (createProgramList programLinesOfFile [])

    let firstError = if null programList then "Нельзя привязать дисциплину к несуществующему учебному плану" else ""
    let errorMessages = filter (not . null) [firstError]
    if null errorMessages
      then do
        let disciplineId = show (getDisciplineId (maximumBy (\a b -> compare (getDisciplineId a) (getDisciplineId b)) disciplineList) + 1)
        customWriteFile _DB_DISCIPLINE_FILE_NAME _DB_DISCIPLINE_TEMP_FILE_NAME (unlines (disciplineLinesOfFile ++ ["Discipline " ++ disciplineId ++ " \"" ++ disciplineTitle ++ "\" " ++ disciplineProgramId]))
        putStrLn $ "Дисциплина " ++ disciplineTitle ++ " была создана."
      else do
        mapM_ printError errorMessages
