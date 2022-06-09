module Lib3 (someFunc) where

import Data.List (sortBy)
import Text.ParserCombinators.Parsec

data Dir = Dir {dir_size :: Int, dir_name :: String} deriving (Show)

data DirPack = DirPack {pack_size :: Int, dirs :: [Dir]} deriving (Show)

mediaSize = 700 * 1024 * 1024

greedyPack dirs = foldl maybeAddDir (DirPack 0 []) $ sortBy cmpSize dirs
  where
    cmpSize d1 d2 = compare (dir_size d1) (dir_size d2)

maybeAddDir p d =
  let new_size = pack_size p + dir_size d
      new_dirs = d : dirs p
   in if new_size > mediaSize then p else DirPack new_size new_dirs

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

someFunc :: IO ()
someFunc = do
  input <- getContents
  putStrLn ("Debug: got input " ++ input)
  let dirs = case parse parseInput "stdin" input of
        Left err -> error $ "\n#Input: \n" ++ show input ++ "\n#Error:\n" ++ show err
        Right result -> result
  putStrLn "solution:"
  print (greedyPack dirs)
