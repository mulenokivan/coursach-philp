module TBUI.Routes (
  routes
) where
  import TBUI.Tools (clearScreen)
  import TBUI.Menus.StartMenu (startMenu)
  import TBUI.Menus.SpecialityMenu (specialityMenu)

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
      "Exit" -> do
        putStrLn "Выход"
        return ()
      _ -> do
        routes "StartMenu"
