module TBUI.Menus.ProgramMenu (
  programMenu
) where
  -- LIBRARIES
  import Text.Read (readMaybe)
  import Data.Maybe (fromJust)
  import Data.Char (isDigit)
  import Data.List (maximumBy, intercalate)
  import qualified System.Console.ANSI as ANSI

  -- MODELS
  import Models.Program

  -- MODULES
  import Modules.File
  import Modules.Search
  import Modules.ReadDB

  -- TBUI
  import TBUI.Tools

  _PAGINATE_PER :: Int
  _PAGINATE_PER = 6

  programMenu :: IO (String, Integer)
  programMenu = do
    printHeader "Учебные планы"

    contents <- customReadFile _DB_PROGRAM_FILE_NAME
    let linesOfFile = lines contents
    let programList = createProgramList linesOfFile []

    mapM_ (\program -> do
        printProgram program
      ) programList

    printNoticeList [
      "[1] Главное меню",
      "Чтобы создать учебный план, напишите: 'Create <название> <id направления подготовки>' "
      ]

    input <- getLine
    stringOperations input linesOfFile programList

  stringOperations :: String -> [String] -> [Program] -> IO (String, Integer)
  stringOperations inputValue linesOfFile programList
    | (findSubStrIdx inputValue "[1]" 0 /= Nothing) = do
      return ("StartMenu", 1)
    | (findSubStrIdx inputValue "Goto" 0 /= Nothing) = do
      let [_, id] = reverseArray (removeQuotesFromArray (splitOnQuotes inputValue [] []))
      return ("ProgramInnerMenu", read id :: Integer)
    | (findSubStrIdx inputValue "Create" 0 /= Nothing) = do
      let [_, titleString, specialityIdString] = reverseArray (removeQuotesFromArray (splitOnQuotes inputValue [] []))
      createProgram titleString specialityIdString
      return ("StartMenu", 1)
    | otherwise = do
      clearScreen
      programMenu

  createProgram :: String -> String -> IO ()
  createProgram programTitle programSpecialityId = do
    contents <- customReadFile _DB_PROGRAM_FILE_NAME
    let linesOfFile = lines contents
    let programList = createProgramList linesOfFile []
    let programId = show (getProgramId (maximumBy (\a b -> compare (getProgramId a) (getProgramId b)) programList) + 1)
    customWriteFile _DB_PROGRAM_FILE_NAME _DB_PROGRAM_TEMP_FILE_NAME (unlines (linesOfFile ++ ["Program " ++ programId ++ " \"" ++ programTitle ++ "\" " ++ programSpecialityId]))
