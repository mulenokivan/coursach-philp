module Modules.ReadDB (
  splitOnQuotes,
  removeQuotesFromArray,
  reverseArray,
  wordsWhen,
  findLineById
) where
  -- OUTER METHODS
  splitOnQuotes :: String -> String -> [String] -> [String]
  splitOnQuotes [] current acc = reverse (current : acc)
  splitOnQuotes (x:xs) current acc
    | x == '"' = splitOnQuotesInQuotes xs (x:current) acc
    | x == ' ' = splitOnQuotes xs [] (current : acc)
    | otherwise = splitOnQuotes xs (x:current) acc

  findLineById :: String -> Integer -> Bool
  findLineById line specialityId = do
    let array = splitOnQuotes line [] []
    let idString = array !! 1
    let test = reverse idString
    let idInt = read test :: Integer
    specialityId /= idInt

  removeQuotesFromArray :: [String] -> [String]
  removeQuotesFromArray = map removeQuotes

  reverseArray :: [String] -> [String]
  reverseArray = map reverse

  wordsWhen :: (Char -> Bool) -> String -> [String]
  wordsWhen p s =
    case dropWhile p s of
      "" -> []
      partLine -> leftPartLine : wordsWhen p rightPartLine

        where
          (leftPartLine, rightPartLine) = break p partLine

  -- INNER METHODS
  splitOnQuotesInQuotes :: String -> String -> [String] -> [String]
  splitOnQuotesInQuotes [] current acc = reverse (current : acc)
  splitOnQuotesInQuotes (x:xs) current acc
    | x == '"' = splitOnQuotes xs (x:current) acc
    | otherwise = splitOnQuotesInQuotes xs (x:current) acc

  removeQuotes :: String -> String
  removeQuotes = filter (/= '"')