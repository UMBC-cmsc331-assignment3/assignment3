-- File name: MatchGroups.hs
-- Project:   cmsc331 assignment 3
-- Authors: Steven Hudak
--          Christopher Blake
--          Tae Song
--          Patrick Bettinger

{-# OPTIONS_GHC -Wall -Werror #-}
module Main where

import System.IO
import System.Environment
import System.Exit

-- main is used just to process the comman line args and ship them to
-- the attempGrouping function which is the main workhorse of the
-- program:
main :: IO ()
main = do
    args <- getArgs
    case args of
        [arg1, arg2] | [numStudents, maxGroupSize] <-
                       [read arg1 :: Int, read arg2 :: Int] -> do
                       
            -- This is just for debugging to see what the values are:
            putStrLn $ "Number of Students: " ++ (show numStudents)
            putStrLn $ "Maximum Group Size: " ++ (show maxGroupSize)

            -- The workhorse of the matching algorithm:
            attemptGrouping numStudents maxGroupSize
            
        _ -> do
            name <- getProgName
            hPutStrLn stderr $ "Error: incorrect program input"
            hPutStrLn stderr $ "Usage: " ++ name ++ " <integer> <integer>"
            exitFailure

attemptGrouping :: Int -> Int -> IO ()
attemptGrouping s g
    | s < 1 || g < 1 || (not $ g `evenlyDivides` s) = putStrLn failString
    | otherwise = if result
                  then putStrLn $ "Successful Matching!"
                  else putStrLn failString
    where 
        numProjects = 8
        
        -- Maps fillProject over each project number, to create a list
        -- of Bool values and then foldr combines those to a single Bool
        -- result. (&&) short-circuits so the foldr will stop early if it
        -- finds a False value:
        result = foldr (&&) True (map (fillProject 0) [1..numProjects])
        failString = "Cannot pair up " ++ (show s)
                     ++ " students into groups of "
                     ++ (show g)
                     ++ " across eight assignments"

-- This will attempt to fill an individual project. It doesn't do
-- anything yet, the comparison is just a placeholder to output a Bool:
fillProject :: Int -> Int -> Bool
fillProject n m = n < m

evenlyDivides :: Int -> Int -> Bool
evenlyDivides m n = r == 0
    where
        (_, r) = divMod n m
