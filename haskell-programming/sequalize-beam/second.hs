

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
  let list = map (func mp) (words str)
  putStrLn $ unwords list
  let opName = fetchOpName str
  let tableName = fetchTableName str
  let fieldCompareMap = getFieldMap str
  let query = buildQuery opName tableName fieldCompareMap
  where

    fetchOpName :: String -> String
    fetchOpName str = head $ words str

    fetchTableName :: String -> String
    fetchTableName str = head . tail $ tail  $ dropWhile (/="::") $ words str

    getFieldMap :: String -> String
    getFieldMap str =  let query


    func :: Map.Map String String -> String -> String
    func mp str = case (mp !? str) of
                    Nothing -> str
                    Just _ -> (mp ! str)

