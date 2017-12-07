{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text    as T
import qualified Data.Text.IO as T
import           System.Environment

main :: IO ()
main = mapM_ procFile =<< getArgs

procFile :: String -> IO ()
procFile fn = T.writeFile fn . T.replace "\n\n\n" "\n\n" =<< T.readFile fn
