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
      "Чтобы создать учебный план, напишите: 'Create <название> <id учебного плана>' "
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
      return "StartMenu"
    | otherwise = do
      clearScreen
      disciplineMenu

  createDiscipline :: String -> String -> IO ()
  createDiscipline disciplineTitle disciplineProgramId = do
    contents <- customReadFile _DB_DISCIPLINE_FILE_NAME
    let linesOfFile = lines contents
    let disciplineList = createDisciplineList linesOfFile []
    let disciplineId = show (getDisciplineId (maximumBy (\a b -> compare (getDisciplineId a) (getDisciplineId b)) disciplineList) + 1)
    customWriteFile _DB_DISCIPLINE_FILE_NAME _DB_DISCIPLINE_TEMP_FILE_NAME (unlines (linesOfFile ++ ["Discipline " ++ disciplineId ++ " \"" ++ disciplineTitle ++ "\" " ++ disciplineProgramId]))
