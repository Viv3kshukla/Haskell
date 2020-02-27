
import Data.List
main :: IO()
main = do
        let list = ["first", "second", "third", "fourth"]
            value = elemIndex "second" list
            intValue = maybe 0 id $ value
        putStrLn (show intValue)