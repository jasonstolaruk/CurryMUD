{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import           System.Environment


main :: IO ()
main = mapM_ procFile =<< getArgs


procFile :: String -> IO ()
procFile fn = writeFile fn . T.replace "\n\n" "\n" =<< T.readFile (T.pack fn)
