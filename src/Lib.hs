module Lib
    ( someFunc
    ) where
c = putStrLn "C!"
combine before after =
  do before
     putStrLn "In the middle"
     after
someFunc :: IO ()
someFunc = do
  combine c c
  let b = combine (putStrLn "Hello!") (putStrLn "Bye!")
  let d = combine (b) (combine c c)
  putStrLn "So Long!"
