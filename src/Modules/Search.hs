{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Modules.Search (
  findSpecialityById,
  findProgramById,
  findSemestersByProgramId,
  findDisciplinesByProgramId,
  findDisciplineById,
  findLoadingsByDisciplineId,
  findLoadingsBySemesterId,
  findLoadingsBySemesterIdAndDisciplineId,
  findSemesterById,
  findLoadingById,
  findSubStrIdx
) where
  -- LIBRARIES
  import Text.Read (readMaybe)
  import Data.Maybe (fromJust)

  -- MODELS
  import Models.Speciality
  import Models.Discipline
  import Models.Program
  import Models.Loading
  import Models.Semester

  -- OUTER METHODS

  -- Speciality
  findSpecialityById :: [Speciality] -> Integer -> Speciality
  findSpecialityById (x:xs) specialityId
    | (getSpecialityId x == specialityId) = x
    | otherwise = findSpecialityById xs specialityId

  -- Program
  findProgramById :: [Program] -> Integer -> Program
  findProgramById (x:xs) programId
    | (getProgramId x == programId) = x
    | otherwise = findProgramById xs programId

  -- Semester
  findSemestersByProgramId :: [Semester] -> Integer -> [Semester]
  findSemestersByProgramId [] _ = []
  findSemestersByProgramId (x:xs) programId
    | (getSemesterProgramId x == programId) = x : findSemestersByProgramId xs programId
    | otherwise = findSemestersByProgramId xs programId

  findSemesterById :: [Semester] -> Integer -> Semester
  findSemesterById (x:xs) semesterId
    | (getSemesterId x == semesterId) = x
    | otherwise = findSemesterById xs semesterId

  --Discipline
  findDisciplineById :: [Discipline] -> Integer -> Discipline
  findDisciplineById (x:xs) disciplineId
    | (getDisciplineId x == disciplineId) = x
    | otherwise = findDisciplineById xs disciplineId

  findDisciplinesByProgramId :: [Discipline] -> Integer -> [Discipline]
  findDisciplinesByProgramId [] _ = []
  findDisciplinesByProgramId (x:xs) programId
    | (getDisciplineProgramId x == programId) = x : findDisciplinesByProgramId xs programId
    | otherwise = findDisciplinesByProgramId xs programId

  --Loading
  findLoadingById :: [Loading] -> Integer -> Loading
  findLoadingById (x:xs) loadingId
    | (getLoadingId x == loadingId) = x
    | otherwise = findLoadingById xs loadingId
  findLoadingsByDisciplineId :: [Loading] -> Integer -> [Loading]
  findLoadingsByDisciplineId [] _ = []
  findLoadingsByDisciplineId (x:xs) disciplineId
    | (getLoadingDisciplineId x == disciplineId) = x : findLoadingsByDisciplineId xs disciplineId
    | otherwise = findLoadingsByDisciplineId xs disciplineId

  findLoadingsBySemesterId :: [Loading] -> Integer -> [Loading]
  findLoadingsBySemesterId [] _ = []
  findLoadingsBySemesterId (x:xs) semesterId
    | (getLoadingSemesterId x == semesterId) = x : findLoadingsBySemesterId xs semesterId
    | otherwise = findLoadingsBySemesterId xs semesterId

  findLoadingsBySemesterIdAndDisciplineId :: [Loading] -> Integer -> Integer -> [Loading]
  findLoadingsBySemesterIdAndDisciplineId [] _ _ = []
  findLoadingsBySemesterIdAndDisciplineId (x:xs) semesterId disciplineId
    | (getLoadingSemesterId x == semesterId) && (getLoadingDisciplineId x == disciplineId) = x : findLoadingsBySemesterIdAndDisciplineId xs semesterId disciplineId
    | otherwise = findLoadingsBySemesterIdAndDisciplineId xs semesterId disciplineId

  -- Other
  findSubStrIdx :: String -> String -> Integer -> Maybe Integer
  findSubStrIdx "" _ _ = Nothing
  findSubStrIdx s target n
    | take (length target) s == target = Just n
    | otherwise = findSubStrIdx (tail s) target (n + 1)

  -- INNER METHODS