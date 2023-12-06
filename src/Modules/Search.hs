module Modules.Search (
  findSpecialityById,
  findSubStrIdx
) where
  -- LIBRARIES
  import Text.Read (readMaybe)
  import Data.Maybe (fromJust)

  -- MODELS
  import Models.Speciality

  -- OUTER METHODS

  -- Speciality
  findSpecialityById :: [Speciality] -> Integer -> Speciality
  findSpecialityById (x:xs) specialityId
    | (getSpecialityId x == specialityId) = x
    | otherwise = findSpecialityById xs specialityId

  -- Other
  findSubStrIdx :: String -> String -> Integer -> Maybe Integer
  findSubStrIdx "" _ _ = Nothing
  findSubStrIdx s target n
    | take (length target) s == target = Just n
    | otherwise = findSubStrIdx (tail s) target (n + 1)

  -- INNER METHODS