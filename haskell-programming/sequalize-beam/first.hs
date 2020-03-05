{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Data.List
import Data.Map ((!?), (!))
import qualified Data.Text as T
import qualified Data.Map as Map

transform :: IO ()
transform = do
  str <- readFile "file.txt"
  putStrLn str

  let keys = ["/\\", "String", "Int", "where_",":=","==?","\"","WHERE", "And","AND","and","Or"]
  let values = ["==.","B.val_","B.val_", "\\rec","->", "==.","","rec","&&.","&&.","&&.","||."]

  let keyValueList = zip keys values
  let mp = Map.fromList keyValueList
  putStrLn $ show (words str)
  let opName = fetchOpName str

  let tableName = fetchTableName str

  let queryTail = drop 2 (words str)

  let newStr = unwords $ takeWhile (/= "::") (queryTail)

  let queryString = getQueryString newStr

  let repCntString = replaceCharCnt '"' 1 queryString
--  let repString  = replaceChar '"' ' ' queryString
--  let splitQString = splitQueryString "\"" newStr


--  putStrLn $ "logging out the splitQString " ++ (show splitQString)

--  let queryList = map (func mp) (words repCntString)
  let queryList = words repCntString
  putStrLn $ "logging out the finalList " ++ (unwords queryList)

--  putStrLn $ "logging out the list " ++ (show queryList)
  let newQueryList = processAndOr "And" queryList
  let finalQueryList = map (func mp) (newQueryList)
  putStrLn $ "logging out the newQueryList " ++ (show finalQueryList)
--  let finalList = (opName : tableName : "$ \\rec -> " : queryList)
  let finalList = (opName : tableName : "( \\rec -> " : finalQueryList)
  let finalString = replaceChar ']' ')' $ replaceChar '[' '(' $ replaceChar ',' ' ' (unwords finalList)



  putStrLn finalString
  where

    fetchOpName :: String -> String
    fetchOpName str = head $ words str

    fetchTableName :: String -> String
    fetchTableName str = head . tail $ tail  $ dropWhile (/="::") $ words str

    getQueryString :: String -> String
    getQueryString str = let list = tail $ dropWhile (/= '[') (str)
--                             newList = takeWhile (/= ']') list
                         in (list)



    processAndOr :: String -> [String] -> [String]
    processAndOr opr [] = []
    processAndOr opr (x:[]) = [x]
    processAndOr opr (x:y:xs)
      | (last x) == ',' = (x : opr : y : (processAndOr opr xs ))
      | (last y) == ',' = (x : y : opr : (processAndOr opr xs ))
      | (head y) == '[' = (y : (processAndOr x xs ))
      | otherwise = (x : y : processAndOr opr (xs))


    replaceChar :: Char -> Char -> String -> String
    replaceChar rep with [] = []
    replaceChar rep with (x:xs) =
      if x == rep
        then (with : (replaceChar rep with (xs)))
        else (x: (replaceChar rep with (xs)))

    replaceCharCnt :: Char -> Int -> String -> String
    replaceCharCnt rep cnt [] = []
    replaceCharCnt rep cnt (x:xs) =
      if x == rep
        then if cnt `mod` 2 == 1
              then ('r':'e':'c':' ':'^':'.' : (replaceCharCnt rep (cnt+1) (xs)))
              else (' ': (replaceCharCnt rep (cnt+1) (xs)))
        else (x: (replaceCharCnt rep cnt (xs)))

    splitQueryString :: String -> String -> [String]
    splitQueryString delm query = map (T.unpack) (T.splitOn (T.pack delm) (T.pack query))

    replace :: Eq a => [a] -> [a] -> [a] -> [a]
    replace [] _ _ = []
    replace s find repl =
        if take (length find) s == find
            then repl ++ (replace (drop (length find) s) find repl)
            else [head s] ++ (replace (tail s) find repl)


    func :: Map.Map String String -> String -> String
    func mp str = case (mp !? str) of
                    Nothing -> str
                    Just _ -> (mp ! str)

