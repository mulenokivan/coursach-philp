module TBUI.Menus.SpecialityMenu (
  specialityMenu
) where
  -- LIBRARIES
  import Text.Read (readMaybe)
  import Data.Maybe (fromJust)
  import Data.Char (isDigit)
  import Data.List (maximumBy, intercalate)
  import qualified System.Console.ANSI as ANSI

  -- MODELS
  import Models.Speciality

  -- MODULES
  import Modules.File (customReadFile, customWriteFile)
  import Modules.Search (findSubStrIdx, findSpecialityById)
  import Modules.ReadDB

  -- TBUI
  import TBUI.Tools (clearScreen, printError, printHeader, printNoticeList)

  _PAGINATE_PER :: Int
  _PAGINATE_PER = 6

  specialityMenu :: IO (String)
  specialityMenu = do
    printHeader "Направления подготовки"

    contents <- customReadFile _DB_SPECIALITY_FILE_NAME
    let linesOfFile = lines contents
    let specialityList = createSpecialityList linesOfFile []

    mapM_ (\speciality -> do
        printSpeciality speciality
      ) specialityList

    printNoticeList [
      "Чтобы выйти в главное меню, напишите: 'Exit'",
      "Чтобы создать направление подготовки, напишите: 'Create <название> <код>' "
      ]

    input <- getLine
    stringOperations input linesOfFile specialityList

  stringOperations :: String -> [String] -> [Speciality] -> IO (String)
  stringOperations inputValue linesOfFile specialityList
    | (findSubStrIdx inputValue "Exit" 0 /= Nothing) = do
      return "StartMenu"
    | (findSubStrIdx inputValue "Create" 0 /= Nothing) = do
      let [_, titleString, codeString] = reverseArray (removeQuotesFromArray (splitOnQuotes inputValue [] []))
      createSpeciality titleString codeString
      return "StartMenu"
    | otherwise = do
      clearScreen
      specialityMenu

  createSpeciality :: String -> String -> IO ()
  createSpeciality specialityTitle specialityCode = do
    contents <- customReadFile _DB_SPECIALITY_FILE_NAME
    let linesOfFile = lines contents
    let specialityList = createSpecialityList linesOfFile []
    let specialityId = show (getSpecialityId (maximumBy (\a b -> compare (getSpecialityId a) (getSpecialityId b)) specialityList) + 1)
    customWriteFile _DB_SPECIALITY_FILE_NAME _DB_SPECIALITY_TEMP_FILE_NAME (unlines (linesOfFile ++ ["Speciality " ++ specialityId ++ " \"" ++ specialityTitle ++ "\" \"" ++ specialityCode ++ "\""]))
