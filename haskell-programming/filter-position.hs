

f :: [Int] -> [Int]
f [] = []
f [x] = [x]
f (x:y:xs) = y : f xs

main :: IO()
main = do
        inputData <- getContents
        mapM_ (putStrLn. show). f. map read. lines $ inputData