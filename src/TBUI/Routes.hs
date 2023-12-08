module TBUI.Routes (
  routes
) where
  import TBUI.Tools (clearScreen)
  import TBUI.Menus.StartMenu (startMenu)
  import TBUI.Menus.SpecialityMenu (specialityMenu)
  import TBUI.Menus.ProgramMenu (programMenu)
  import TBUI.Menus.ProgramInnerMenu (programInnerMenu)
  import TBUI.Menus.SemesterMenu (semesterMenu)
  import TBUI.Menus.DisciplineMenu (disciplineMenu)
  import TBUI.Menus.LoadingMenu (loadingMenu)

  routes :: (String, Integer) -> IO ()
  routes (route, id) = do
    print route
    case route of
      "StartMenu" -> do
        clearScreen
        newRoute <- startMenu
        routes (createEmptyState newRoute)
      "SpecialityMenu" -> do
        clearScreen
        newRoute <- specialityMenu
        routes (createEmptyState newRoute)
      "ProgramMenu" -> do
        clearScreen
        newRoute <- programMenu
        routes newRoute
      "ProgramInnerMenu" -> do
        clearScreen
        newRoute <- programInnerMenu id
        routes newRoute
      "SemesterMenu" -> do
        clearScreen
        newRoute <- semesterMenu 1
        routes (createEmptyState newRoute)
      "DisciplineMenu" -> do
        clearScreen
        newRoute <- disciplineMenu
        routes (createEmptyState newRoute)
      "LoadingMenu" -> do
        clearScreen
        newRoute <- loadingMenu
        routes (createEmptyState newRoute)
      "Exit" -> do
        putStrLn "Выход"
        return ()
      _ -> do
        routes (createEmptyState "StartMenu")

  createEmptyState :: String -> (String, Integer)
  createEmptyState route = (route, 0)