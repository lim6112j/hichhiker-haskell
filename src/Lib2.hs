-- module Lib2
--   ( someFunc,
--   )
-- where

import Text.ParserCombinators.Parsec

parseInput =
  do
    dirs <- many dirAndSize
    eof :: Parser ()
    return dirs

data Dir = Dir Int String deriving (Show)

dirAndSize = do
  size <- many1 digit
  spaces
  dir_name <- anyChar `manyTill` newline
  return (Dir (read size) dir_name)

-- someFunc :: IO ()
-- someFunc = do
--   input <- getContents
--   putStrLn ("Debug: got input " ++ input)
--   let dirs = case parse parseInput "stdin" input of
--         Left err -> error $ "Input: \n" ++ show input ++ "\nError:\n" ++ show err
--         Right result -> result
--   putStrLn "Debug: parsed:"
--   print dirs

main :: IO ()
main = do
  input <- getContents
  putStrLn ("Debug: got input " ++ input)
  let dirs = case parse parseInput "stdin" input of
        Left err -> error $ "Input: \n" ++ show input ++ "\nError:\n" ++ show err
        Right result -> result
  putStrLn "Debug: parsed:"
  print dirs
