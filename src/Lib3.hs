module Lib3 where

import Control.Monad (liftM2, replicateM)
import Data.Ix
import Data.List (maximumBy, sortBy)
import Text.ParserCombinators.Parsec
  ( Parser,
    anyChar,
    digit,
    eof,
    many,
    many1,
    manyTill,
    newline,
    parse,
    spaces,
  )

-- data Dir = Dir Int String deriving (Show)
data Dir = Dir {dir_size :: Integer, dir_name :: String} deriving (Show, Eq)

--instance Eq Dir where
--(==) a b = (dir_size a == dir_size b) && (dir_name a == dir_name b)

data DirPack = DirPack {pack_size :: Integer, dirs :: [Dir]} deriving (Show)

mediaSize = 700 * 1024 * 1024 :: Integer

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

precomputedDisksFor :: [Dir] -> [DirPack]
precomputedDisksFor dirs =
  let precomp = map bestDisk [0 ..]
      bestDisk 0 = DirPack 0 []
      bestDisk limit =
        case [ DirPack (dir_size d + s) (d : ds) | d <- filter (inRange (1, limit) . dir_size) dirs, dir_size d > 0, let (DirPack s ds) = precomp !! fromInteger (limit - dir_size d), d `notElem` ds
             ] of
          [] -> DirPack 0 []
          packs -> maximumBy compSize packs
      compSize a b = compare (pack_size a) (pack_size b)
   in precomp

dynamicPack limit dirs = precomputedDisksFor dirs !! fromInteger limit

someFunc :: IO ()
someFunc = do
  input <- getContents
  putStrLn ("Debug: got input " ++ input)
  let dirs = case parse parseInput "stdin" input of
        Left err -> error $ "Input: \n" ++ show input ++ "\nError:\n" ++ show err
        Right result -> result
  putStrLn "Debug: parsed:"
  print dirs
