{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Mud.Misc.CurryTime where

import Mud.Data.Misc
import Mud.Util.Text

import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Time (UTCTime(..), diffUTCTime, fromGregorian, getCurrentTime)
import Prelude hiding (min)


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


showCurryTime :: CurryTime -> [Text]
showCurryTime CurryTime { .. } = [ "Year:         " <> showText  curryYear
                                 , "Month:        " <> showText  curryMonth
                                 , "Week:         " <> showText  curryWeek
                                 , "Day of month: " <> helper    curryDayOfMonth
                                 , "Day of week:  " <> helper    curryDayOfWeek
                                 , "Hour:         " <> showText  curryHour
                                 , "Min:          " <> showText  curryMin
                                 , "Sec:          " <> showText  currySec ]
  where
    helper = (|<>|) <$> mkOrdinal . fromIntegral <*> pp . toEnum


curryEpoch :: UTCTime
curryEpoch = UTCTime (fromGregorian 2016 1 1) 0


getSecsFromCurryEpoch :: IO Sec
getSecsFromCurryEpoch = round . (`diffUTCTime` curryEpoch) <$> getCurrentTime


getCurryTime :: IO CurryTime
getCurryTime = secsToCurryTime <$> getSecsFromCurryEpoch


secsToCurryTime :: Sec -> CurryTime
secsToCurryTime x = let years  = x `div` currySecsInYear
                        months = x `div` currySecsInMonth
                        weeks  = x `div` currySecsInWeek
                        days   = x `div` currySecsInDay
                        hours  = x `div` currySecsInHour
                        mins   = x `div` currySecsInMin
                        secs   = x
                        -----
                        year       =        years + 200
                        month      =        months `rem` curryMonthsInYear
                        week       =        weeks  `rem` curryWeeksInMonth
                        dayOfMonth = succ $ days   `rem` curryDaysInMonth
                        dayOfWeek  = succ $ days   `rem` curryDaysInWeek
                        hour       =        hours  `rem` curryHoursInDay
                        min        =        mins   `rem` curryMinsInHour
                        sec        =        secs   `rem` currySecsInMin
                    in CurryTime year month week dayOfMonth dayOfWeek hour min sec
