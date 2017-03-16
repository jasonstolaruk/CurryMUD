import Data.Char
import Data.List
import System.Directory

main :: IO ()
main = do
    writeFile path' . unlines . snd . foldl' helper (0, []) . lines =<< readFile path
    copyFile path' path
    removeFile path'
  where
    path' = path ++ "'"

path :: FilePath
path = "/Users/jasonstolaruk/CurryMUD/lib/Mud/TheWorld/Zones/AdminZoneIds.hs"

helper :: (Int, [String]) -> String -> (Int, [String])
helper (x, ts) t | " :: Id" `isInfixOf` t = let w                  = last . words $ t
                                                y  | all isDigit w = read w
                                                   | otherwise     = 0
                                                x' | y == 0        = x + 1
                                                   | otherwise     = x + y
                                                (l,  r  )          = break (== '=') t
                                                (l', r' )          = break isDigit r
                                                (_,  r'')          = span  isDigit r'
                                            in (x', ts ++ [ l ++ l' ++ show x ++ r'' ])
                 | otherwise = (x, ts ++ [t])
