{-# LANGUAGE OverloadedStrings #-}

import Data.List
import Data.Map ((!?), (!))
import qualified Data.Text as T
import qualified Data.Map as Map

transform :: IO ()
transform = do
  str <- readFile "file.txt"
  putStrLn str
  let keys = ["/\\", "String", "Int", "where_",":=","==?",",","\"","WHERE"]
  let values = ["==.","B.val_","B.val_", "\\rec","->", "==.","&&.","","rec"]
  let keyValueList = zip keys values
  let mp = Map.fromList keyValueList
  let opName = fetchOpName str
  let tableName = fetchTableName str
  let queryTail = drop 2 (words str)
  let newStr = unwords $ takeWhile (/= "::") (queryTail)
  putStrLn $ newStr
  let queryString = getQueryString newStr
  putStrLn $ "logging out the queryString "  ++ queryString
  let queryList = map (func mp) (words newStr)
  let finalList = (opName : tableName : queryList)
  putStrLn $ unwords finalList
  where

    fetchOpName :: String -> String
    fetchOpName str = head $ words str

    fetchTableName :: String -> String
    fetchTableName str = head . tail $ tail  $ dropWhile (/="::") $ words str

    getQueryString :: String -> String
    getQueryString str = let list = tail $ dropWhile (/= "[") (words str)
                             newList = takeWhile (/= "]") list
                         in (unwords newList)


    func :: Map.Map String String -> String -> String
    func mp str = case (mp !? str) of
                    Nothing -> str
                    Just _ -> (mp ! str)

