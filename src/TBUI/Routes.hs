module TBUI.Routes (
  routes
) where
  import TBUI.Tools (clearScreen)
  import TBUI.Menus.StartMenu (startMenu)
  import TBUI.Menus.SpecialityMenu (specialityMenu)
  import TBUI.Menus.ProgramMenu (programMenu)
  import TBUI.Menus.SemesterMenu (semesterMenu)
  import TBUI.Menus.DisciplineMenu (disciplineMenu)
  import TBUI.Menus.LoadingMenu (loadingMenu)

  routes :: String -> IO ()
  routes route = do
    case route of
      "StartMenu" -> do
        clearScreen
        newRoute <- startMenu
        routes newRoute
      "SpecialityMenu" -> do
        clearScreen
        newRoute <- specialityMenu
        routes newRoute
      "ProgramMenu" -> do
        clearScreen
        newRoute <- programMenu
        routes newRoute
      "SemesterMenu" -> do
        clearScreen
        newRoute <- semesterMenu
        routes newRoute
      "DisciplineMenu" -> do
        clearScreen
        newRoute <- disciplineMenu
        routes newRoute
      "LoadingMenu" -> do
        clearScreen
        newRoute <- loadingMenu
        routes newRoute
      "Exit" -> do
        putStrLn "Выход"
        return ()
      _ -> do
        routes "StartMenu"
