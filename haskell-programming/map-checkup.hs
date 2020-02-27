

import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Text as T

checkMap :: String -> String -> IO ()
checkMap keys values = let keyList = words keys
                           valueList = words values
                           keyValueList = zip keyList valueList
                           keyValueMap = Map.fromList keyValueList
                           result = Map.lookup "vivek" keyValueMap
                           finalValue = case result of
                                          Nothing -> "NotFound"
                                          Just _ -> fromJust result
                       in putStrLn $ "printing out the value " ++ finalValue


main :: IO ()
main = do
        putStrLn "Enter space seperated keys"
        keys <- getLine
        putStrLn "Enter space seperated values"
        values <- getLine
        checkMap keys values
        putStrLn "Operation Completed"


