module Main where

import TBUI.Routes (routes)

main :: IO ()
main = do
  routes ("StartMenu", 0)