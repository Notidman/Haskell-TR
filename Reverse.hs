
reverseLine :: [Char] -> IO ()
reverseLine xs = putStrLn $ reverse xs

main :: IO ()
main = do
     putStrLn "Введите строку: "
     xs <- getLine
     reverseLine xs
     _ <- getLine
     putStrLn ""