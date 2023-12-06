module TBUI.Tools (
  clearScreen,
  printOptions,
  printError,
  printSuccess,
  printHeader,
  printPagination,
  printNoticeList,
) where
  -- LIBRARIES
  import qualified System.Console.ANSI as ANSI
  import Text.Printf (printf)

  -- MODELS
  import Models.Speciality (Speciality)

  -- MODULES
  import Modules.Search (findSpecialityById)

  clearScreen :: IO ()
  clearScreen = do
    ANSI.clearScreen
    ANSI.setCursorPosition 0 0

  printOptions :: [(String, String)] -> IO ()
  printOptions options =
    mapM_ putStrLn optionsString

    where
      optionsString = map (\(s, _) -> s) options

  printError :: String -> IO ()
  printError errorText = do
    ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.White]
    ANSI.setSGR [ANSI.SetColor ANSI.Background ANSI.Vivid ANSI.Red]
    putStrLn (replicate 100 '=')
    putStrLn "Ошибка:"
    putStrLn errorText
    putStrLn (replicate 100 '=')
    ANSI.setSGR [ANSI.Reset]

  printSuccess :: String -> IO ()
  printSuccess successText = do
    ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Green]
    putStrLn (replicate 100 '=')
    putStrLn "Успешно:"
    putStrLn successText
    putStrLn (replicate 100 '=')
    ANSI.setSGR [ANSI.Reset]

  printHeader :: String -> IO ()
  printHeader header = do
    ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Black]
    putStrLn (replicate 100 '=')
    ANSI.setSGR [ANSI.Reset]
    ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue]
    putStrLn header
    ANSI.setSGR [ANSI.Reset]
    ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Black]
    putStrLn (replicate 100 '=')
    ANSI.setSGR [ANSI.Reset]

  printPagination :: Int -> Int -> IO ()
  printPagination pageNumber pages = do
    putStrLn "Страницы"
    putStrLn ("[" ++ (show pageNumber) ++ "/" ++ (show pages) ++ "]")
    ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Black]
    putStrLn (replicate 100 '=')
    ANSI.setSGR [ANSI.Reset]

  printNoticeList :: [String] -> IO ()
  printNoticeList noticeList = do
    ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Black]
    putStrLn (replicate 100 '=')
    putStrLn (replicate 100 '=')
    ANSI.setSGR [ANSI.Reset]
    ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue]
    putStrLn "Заметка:"
    mapM_ putStrLn noticeList
    ANSI.setSGR [ANSI.Reset]
    ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Black]
    putStrLn (replicate 100 '=')
    ANSI.setSGR [ANSI.Reset]