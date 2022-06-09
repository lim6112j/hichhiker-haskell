module Lib2 where

import Control.Monad (liftM2, replicateM)
import Data.List (sortBy)
import Test.QuickCheck
import Text.ParserCombinators.Parsec

-- data Dir = Dir Int String deriving (Show)
data Dir = Dir {dir_size :: Int, dir_name :: String} deriving (Show)

data DirPack = DirPack {pack_size :: Int, dirs :: [Dir]} deriving (Show)

media_size = 700 * 1024 * 1024

greedy_pack dirs = foldl maybe_add_dir (DirPack 0 []) $ sortBy cmpSize dirs
  where
    cmpSize d1 d2 = compare (dir_size d1) (dir_size d2)

maybe_add_dir p d =
  let new_size = pack_size p + dir_size d
      new_dirs = d : (dirs p)
   in if new_size > media_size then p else DirPack new_size new_dirs

parseInput =
  do
    dirs <- many dirAndSize
    eof :: Parser ()
    return dirs

dirAndSize = do
  size <- many1 digit
  spaces
  dir_name <- anyChar `manyTill` newline
  return (Dir (read size) dir_name)

-- instance Arbitrary Dir where
--   arbitrary = liftM2 Dir gen_size gen_name
--     where
--       gen_size = do
--         s <- choose (10, 14000)
--         return (s * 1024 * 1024)
--       gen_name = do
--         n <- choose (1, 300)
--         replicateM n (elements "fubar/")

-- prop_greedy_pack_is_fixpoint ds =
--   let pack = greedy_pack ds
--    in pack_size pack == pack_size (greedy_pack (dirs pack))

someFunc :: IO ()
someFunc = do
  input <- getContents
  putStrLn ("Debug: got input " ++ input)
  let dirs = case parse parseInput "stdin" input of
        Left err -> error $ "Input: \n" ++ show input ++ "\nError:\n" ++ show err
        Right result -> result
  putStrLn "Debug: parsed:"
  print dirs

-- main :: IO ()
-- main = do
--   input <- getContents
--   putStrLn ("Debug: got input " ++ input)
--   let dirs = case parse parseInput "stdin" input of
--         Left err -> error $ "Input: \n" ++ show input ++ "\nError:\n" ++ show err
--         Right result -> result
--   putStrLn "Debug: parsed:"
--   print dirs
--   putStrLn "Solution:"
--   print (greedy_pack dirs)
