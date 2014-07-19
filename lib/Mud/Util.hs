{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings #-}

module Mud.Util ( adjustIndent
                , aOrAn
                , blowUp
                , bracketPad
                , bracketQuote
                , calcIndent
                , countOcc
                , dblQuote
                , dblQuoteStr
                , deleteFirstOfEach
                , dispAssocList
                , dispGenericErrorMsg
                , divider
                , dumpFile
                , dumpFileNoWrapping
                , dumpFileWithDividers
                , eitherRet
                , findFullNameForAbbrev
                , grepTextList
                , isVowel
                , maybeRet
                , maybeNewLine
                , mkCountList
                , mkOrdinal
                , newLine
                , output
                , outputCon
                , outputConIndent
                , outputIndent
                , padOrTrunc
                , parensPad
                , parensQuote
                , patternMatchFail
                , quoteWithAndPad
                , ShouldNewLine
                , showText
                , singleQuote
                , unquote
                , wordWrap
                , wordWrapIndent
                , wordWrapLines
                , wrapLineWithIndentTag
                , xformLeading ) where

import Mud.StateDataTypes
import Mud.TopLvlDefs hiding (cols)
import qualified Mud.TopLvlDefs as T (cols)

import Control.Lens (_1, _2, both, folded, over, to)
import Control.Lens.Operators ((^.), (^..))
import Control.Monad (guard, when)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isDigit, isSpace)
import Data.List (delete, foldl', sort)
import Data.Monoid ((<>))
import Data.Text.Strict.Lens (packed, unpacked)
import qualified Data.Text as T
import qualified Data.Text.IO as T (putStrLn, readFile)


-- ==================================================
-- Error handling:


blowUp :: T.Text -> T.Text -> T.Text -> [T.Text] -> a
blowUp modName funName msg vals = error $ errorMsg^.unpacked
  where
    errorMsg   = T.concat [ modName, " ", funName, ": ", msg, ". ", valsText ]
    valsText   = "Values: " <> listedVals
    listedVals = T.intercalate ", " . map singleQuote $ vals


patternMatchFail :: T.Text -> T.Text -> [T.Text] -> a
patternMatchFail modName funName = blowUp modName funName "pattern match failure"


dispGenericErrorMsg :: MudStack ()
dispGenericErrorMsg = output "Unfortunately, an error occured while executing your command."


-- ==================================================
-- Output:


newLine :: IO ()
newLine = putChar '\n'


type ShouldNewLine = Bool


maybeNewLine :: ShouldNewLine -> MudStack ()
maybeNewLine snl = when snl . liftIO $ newLine


output :: T.Text -> MudStack ()
output = liftIO . mapM_ T.putStrLn . wordWrap T.cols


outputIndent :: Int -> T.Text -> MudStack ()
outputIndent n = liftIO . mapM_ T.putStrLn . wordWrapIndent T.cols n


outputCon :: [T.Text] -> MudStack () -- Prefer over "output" when there would be more than two "<>"s.
outputCon = output . T.concat


outputConIndent :: Int -> [T.Text] -> MudStack ()
outputConIndent n = outputIndent n . T.concat


divider :: IO ()
divider = T.putStrLn . T.replicate T.cols $ "="


dumpFile :: FilePath -> IO () -- TODO: Implement paging.
dumpFile fn = takeADump =<< T.readFile fn
  where
    takeADump = mapM_ T.putStrLn . concat . wordWrapLines T.cols . T.lines


dumpFileWithDividers :: FilePath -> IO ()
dumpFileWithDividers fn = divider >> dumpFile fn >> divider


dumpFileNoWrapping :: FilePath -> IO ()
dumpFileNoWrapping fn = takeADump =<< T.readFile fn
  where
    takeADump = T.putStrLn


dispAssocList :: (Show a, Show b) => [(a, b)] -> IO ()
dispAssocList = mapM_ takeADump
  where
    takeADump (a, b) = mapM_ T.putStrLn . wordWrapIndent T.cols 2 $ (unquote . showText $ a) <> ": " <> showText b


-- ==================================================
-- Word wrapping and indenting:


wordWrap :: Int -> T.Text -> [T.Text]
wordWrap cols t
  | T.null afterMax         = [t]
  | T.any isSpace beforeMax = beforeSpace : wordWrap cols (afterSpace <> afterMax)
  | otherwise               = beforeMax   : wordWrap cols afterMax
  where
    (beforeMax, afterMax)     = T.splitAt cols t
    (beforeSpace, afterSpace) = breakEnd beforeMax


breakEnd :: T.Text -> (T.Text, T.Text)
breakEnd t = over both T.reverse (before, after)
  where (after, before) = T.break isSpace . T.reverse $ t


wordWrapIndent :: Int -> Int -> T.Text -> [T.Text]
wordWrapIndent cols n = map leadingNullsToSpcs . wrapIt . leadingSpcsToNulls
  where
    wrapIt t
      | T.null afterMax         = [t]
      | T.any isSpace beforeMax = beforeSpace : wordWrapIndent cols n (leadingIndent <> afterSpace <> afterMax)
      | otherwise               = beforeMax   : wordWrapIndent cols n (leadingIndent <> afterMax)
      where
        leadingIndent             = T.replicate (adjustIndent n cols) "\NUL"
        (beforeMax, afterMax)     = T.splitAt cols t
        (beforeSpace, afterSpace) = breakEnd beforeMax


leadingSpcsToNulls :: T.Text -> T.Text
leadingSpcsToNulls = xformLeading ' ' '\NUL'


leadingNullsToSpcs :: T.Text -> T.Text
leadingNullsToSpcs = xformLeading '\NUL' ' '


xformLeading :: Char -> Char -> T.Text -> T.Text
xformLeading _ _ "" = ""
xformLeading a b t  = let (as, t') = T.break (/= a) t
                          n        = T.length as
                      in replicate n b ^.packed <> t'


adjustIndent :: Int -> Int -> Int
adjustIndent n cols = if n >= cols then cols - 1 else n


-- TODO: Write tests.
wordWrapLines :: Int -> [T.Text] -> [[T.Text]]
wordWrapLines _    []  = []
wordWrapLines cols [t] = let nolst = numOfLeadingSpcs t
                         in [ wordWrapIndent cols nolst t ]
wordWrapLines cols (a:b:rest) = if T.null a
  then [""] : wrapNext
  else f a  : wrapNext
    where
      wrapNext = wordWrapLines cols $ b : rest
      f
        | hasIndentTag = wrapLineWithIndentTag cols
        | nolsa > 0    = wordWrapIndent cols nolsa
        | nolsb > 0    = wordWrapIndent cols nolsb
        | otherwise    = wordWrap cols
      hasIndentTag = T.last a == indentTagChar
      nolsa        = numOfLeadingSpcs a
      nolsb        = numOfLeadingSpcs b


numOfLeadingSpcs :: T.Text -> Int
numOfLeadingSpcs = T.length . T.takeWhile isSpace


wrapLineWithIndentTag :: Int -> T.Text -> [T.Text]
wrapLineWithIndentTag cols t = wordWrapIndent cols n' t'
  where
    parseIndentTag = T.break (not . isDigit) . T.reverse . T.init $ t
    (numText, t')  = over both T.reverse parseIndentTag
    readsRes       = reads $ numText^.unpacked :: [(Int, String)]
    n              = case readsRes of []       -> 0
                                      [(x, _)] -> x
                                      xs       -> patternMatchFail "Mud.Util" "wrapLineWithIndentTag n" [ showText xs ]
    n'             = if n == 0 then calcIndent t' else adjustIndent n cols


calcIndent :: T.Text -> Int
calcIndent t
  | T.null b  = 0
  | otherwise = lenOfFirstWord + numOfFollowingSpcs
  where
    (a, b)             = T.break isSpace t
    lenOfFirstWord     = T.length a
    numOfFollowingSpcs = numOfLeadingSpcs b


-- ==================================================
-- Quoting:


quoteWith :: (T.Text, T.Text) -> T.Text -> T.Text
quoteWith (a, b) t = T.concat [ a, t, b ]


singleQuote :: T.Text -> T.Text
singleQuote = quoteWith ("'", "'")


dblQuote :: T.Text -> T.Text
dblQuote = quoteWith ("\"", "\"")


dblQuoteStr :: String -> String
dblQuoteStr = (^.unpacked) . dblQuote . (^.packed)


bracketQuote :: T.Text -> T.Text
bracketQuote = quoteWith ("[", "]")


parensQuote :: T.Text -> T.Text
parensQuote = quoteWith ("(", ")")


unquote :: T.Text -> T.Text
unquote = T.init . T.tail


-- ==================================================
-- Padding:


quoteWithAndPad :: (T.Text, T.Text) -> Int -> T.Text -> T.Text
quoteWithAndPad q x t = quoteWith q t' <> T.replicate p " "
  where
    t' = T.take (x - l - 1) t
    l  = sum $ [q^._1, q^._2]^..folded.to T.length
    p  = x - T.length t' - 2


bracketPad :: Int -> T.Text -> T.Text
bracketPad = quoteWithAndPad ("[", "]")


parensPad :: Int -> T.Text -> T.Text
parensPad = quoteWithAndPad ("(", ")")


padOrTrunc :: Int -> T.Text -> T.Text
padOrTrunc x t
  | x < 0 = ""
  | otherwise = let l  = T.length t
                in case l `compare` x of EQ -> t
                                         LT -> let diff = x - l in t <> T.replicate diff " "
                                         GT -> T.take x t


-- ==================================================
-- Misc.:


showText :: (Show a) => a -> T.Text
showText t = t^.to show.packed


aOrAn :: T.Text -> T.Text
aOrAn t | T.null t' = ""
        | isVowel . T.head $ t' = "an " <> t'
        | otherwise = "a " <> t'
  where
    t' = T.strip t


isVowel :: Char -> Bool
isVowel c = c `elem` "aeiou"^.unpacked


findFullNameForAbbrev :: T.Text -> [T.Text] -> Maybe T.Text
findFullNameForAbbrev needle hay = guard (not . null $ res) >> (Just . head $ res)
  where
    res = sort . filter (needle `T.isPrefixOf`) $ hay


countOcc :: (Eq a) => a -> [a] -> Int
countOcc needle = foldl' (\acc x -> if x == needle then succ acc else acc) 0


deleteFirstOfEach :: (Eq a) => [a] -> [a] -> [a]
deleteFirstOfEach delThese fromThis = foldl' (flip delete) fromThis delThese


mkOrdinal :: Int -> T.Text
mkOrdinal 11 = "11th"
mkOrdinal 12 = "12th"
mkOrdinal 13 = "13th"
mkOrdinal x  = let t = showText x
               in t <> case T.last t of '1' -> "st"
                                        '2' -> "nd"
                                        '3' -> "rd"
                                        _   -> "th"


mkCountList :: (Eq a) => [a] -> [Int]
mkCountList xs = map (`countOcc` xs) xs


grepTextList :: T.Text -> [T.Text] -> [T.Text]
grepTextList needle = filter (needle `T.isInfixOf`)


maybeRet :: (Monad m) => (a -> m ()) -> Maybe a -> m ()
maybeRet = maybe (return ())


eitherRet :: (Monad m) => (a -> m b) -> Either a b -> m b
eitherRet = flip either return
