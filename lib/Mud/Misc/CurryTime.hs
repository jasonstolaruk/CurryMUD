{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Mud.Misc.CurryTime where

import Mud.Data.Misc
import Mud.Util.Operators
import Mud.Util.Quoting
import Mud.Util.Text

import Data.Ix (inRange)
import Data.List (lookup)
import Data.Monoid (Sum(..), (<>))
import Data.Text (Text)
import Data.Time (UTCTime(..), diffUTCTime, fromGregorian, getCurrentTime)
import Prelude hiding (min)


initCurryYear :: Year
initCurryYear = 4171 -- Approximately the same number of seconds as 1600 earth years.


-- ==================================================


-- Months
curryMonthsInYear :: Month
curryMonthsInYear = 8


-- Weeks
curryWeeksInMonth, curryWeeksInYear :: Week
curryWeeksInMonth = 3
curryWeeksInYear  = curryWeeksInMonth * curryMonthsInYear


-- Days
curryDaysInWeek, curryDaysInMonth, curryDaysInYear :: Day
curryDaysInWeek  = 7
curryDaysInMonth = curryDaysInWeek  * curryWeeksInMonth
curryDaysInYear  = curryDaysInMonth * curryMonthsInYear


-- Hours
curryHoursInDay, curryHoursInWeek, curryHoursInMonth, curryHoursInYear :: Hour
curryHoursInDay   = 20
curryHoursInWeek  = curryHoursInDay   * curryDaysInWeek
curryHoursInMonth = curryHoursInWeek  * curryWeeksInMonth
curryHoursInYear  = curryHoursInMonth * curryMonthsInYear


-- Mins
curryMinsInHour, curryMinsInDay, curryMinsInWeek, curryMinsInMonth, curryMinsInYear :: Min
curryMinsInHour  = 60
curryMinsInDay   = curryMinsInHour  * curryHoursInDay
curryMinsInWeek  = curryMinsInDay   * curryDaysInWeek
curryMinsInMonth = curryMinsInWeek  * curryWeeksInMonth
curryMinsInYear  = curryMinsInMonth * curryMonthsInYear


-- Seconds
currySecsInMin, currySecsInHour, currySecsInDay, currySecsInWeek, currySecsInMonth, currySecsInYear :: Sec
currySecsInMin = 60
currySecsInHour  = currySecsInMin   * curryMinsInHour   -- 3,600
currySecsInDay   = currySecsInHour  * curryHoursInDay   -- 72,000
currySecsInWeek  = currySecsInDay   * curryDaysInWeek   -- 504,000
currySecsInMonth = currySecsInWeek  * curryWeeksInMonth -- 1,512,000
currySecsInYear  = currySecsInMonth * curryMonthsInYear -- 12,096,000


-- ==================================================


curryEpoch :: UTCTime
curryEpoch = UTCTime (fromGregorian 2017 1 1) 0


-----


getCurryTime :: IO CurryTime
getCurryTime = secsToCurryTime <$> getSecsFromCurryEpoch


-----


getMoonPhaseForDayOfMonth :: Day -> Maybe MoonPhase
getMoonPhaseForDayOfMonth = flip lookup [ (1,  NewMoon)
                                        , (2,  NewMoon)
                                        , (3,  WaxingCrescent)
                                        , (4,  WaxingCrescent)
                                        , (5,  FirstQuarter)
                                        , (6,  FirstQuarter)
                                        , (7,  FirstQuarter)
                                        , (8,  WaxingGibbous)
                                        , (9,  WaxingGibbous)
                                        , (10, WaxingGibbous)
                                        , (11, FullMoon)
                                        , (12, FullMoon)
                                        , (13, FullMoon)
                                        , (14, WaningGibbous)
                                        , (15, WaningGibbous)
                                        , (16, WaningGibbous)
                                        , (17, ThirdQuarter)
                                        , (18, ThirdQuarter)
                                        , (19, ThirdQuarter)
                                        , (20, WaningCrescent)
                                        , (21, WaningCrescent) ]


-----


getSecsFromCurryEpoch :: IO Sec
getSecsFromCurryEpoch = round . (`diffUTCTime` curryEpoch) <$> getCurrentTime


-----


isDay :: Hour -> Bool -- Day (light) hours are 6:00-17:59 (12 hours).
isDay = inRange (6, 17)


isNight :: Hour -> Bool
isNight = not . isDay -- Night (dark) hours are 18:00-5:59 (8 hours).


-----


ppMonthForMonthNum :: Month -> Text
ppMonthForMonthNum = pp . (toEnum :: Int -> CurryMonth) . pred


-----


ppWeekdayForDayOfWeek :: Day -> Text
ppWeekdayForDayOfWeek = pp . (toEnum :: Int -> CurryWeekday) . pred


-----


secsToCurryTime :: Sec -> CurryTime
secsToCurryTime x = let years      = x `div` currySecsInYear
                        months     = x `div` currySecsInMonth
                        weeks      = x `div` currySecsInWeek
                        days       = x `div` currySecsInDay
                        hours      = x `div` currySecsInHour
                        mins       = x `div` currySecsInMin
                        secs       = x
                        -----
                        year       = initCurryYear + years
                        month      = succ $ months `rem` curryMonthsInYear
                        week       = succ $ weeks  `rem` curryWeeksInMonth
                        dayOfMonth = succ $ days   `rem` curryDaysInMonth
                        dayOfWeek  = succ $ days   `rem` curryDaysInWeek
                        hour       =        hours  `rem` curryHoursInDay
                        min        =        mins   `rem` curryMinsInHour
                        sec        =        secs   `rem` currySecsInMin
                    in CurryTime year month week dayOfMonth dayOfWeek hour min sec


-----


showElapsedCurryTime :: UTCTime -> UTCTime -> Text
showElapsedCurryTime a b = let CurryTime { .. } = secsToCurryTime . round $ a `diffUTCTime` b
                               f t x            = Sum x |!| (showTxt x <> spcL t <> sOnNon1 x)
                               ts               = [ f "year"         $ curryYear - initCurryYear
                                                  , f "month" . pred $ curryMonth
                                                  , f "week"  . pred $ curryWeek
                                                  , f "day"   . pred $ curryDayOfMonth
                                                  , f "hour"           curryHour
                                                  , f "minute"         curryMin ]
                           in case reverse . dropBlanks $ ts of []       -> ""
                                                                [x]      -> x
                                                                [y, x]   -> quoteWith' (x, y) . spaced $ "and"
                                                                (x:rest) -> commas . reverse $ "and " <> x : rest



-----


showCurryTime :: CurryTime -> [Text]
showCurryTime CurryTime { .. } = [ "Year:         " <> showTxt   curryYear
                                 , "Month:        " <> helper    ppMonthForMonthNum    curryMonth
                                 , "Week:         " <> mkOrdinal curryWeek
                                 , "Day of month: " <> mkOrdinal curryDayOfMonth
                                 , "Day of week:  " <> helper    ppWeekdayForDayOfWeek curryDayOfWeek
                                 , "Hour:         " <> showTxt   curryHour
                                 , "Min:          " <> showTxt   curryMin
                                 , "Sec:          " <> showTxt   currySec ]
  where
    helper f = (|<>|) <$> mkOrdinal <*> parensQuote . f
