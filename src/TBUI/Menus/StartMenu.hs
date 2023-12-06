module TBUI.Menus.StartMenu (
  startMenu,
  clearScreen
) where
  -- LIBRARIES
  import Data.Char (isDigit)
  import System.IO.Unsafe (unsafePerformIO)
  import qualified System.Console.ANSI as ANSI

  -- TBUI
  import TBUI.Tools (clearScreen, printOptions, printHeader, printError)

  startMenu :: IO (String)
  startMenu = do
    printHeader "Главное меню"
    putStrLn ""

    let options = [
          ("[1] Направления подготовки", "SpecialityMenu"),
          ("[2] Учебные планы", "ProgramMenu"),
          ("[3] Семестры", "SemesterMenu"),
          ("[4] Дисциплины", "DisciplineMenu"),
          ("[5] Выйти", "Exit")
          ]
    printOptions options

    input <- getLine
    if all isDigit input
      then do
        let index = read input :: Int
        let menuName = unsafePerformIO (getOptionByIndex index options "Главное меню")
        return menuName
      else do
        clearScreen
        printError "Ошибка при обработке команды"
        startMenu

  getOptionByIndex :: Int -> [(String, String)] -> String -> IO (String)
  getOptionByIndex index options header
    | (index <= length options) = do
      let (_, menuName) = options!! (index - 1)
      return menuName
    | otherwise = do
      clearScreen
      printError "Ошибка при обработке команды"
      startMenu