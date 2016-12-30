{-# LANGUAGE OverloadedStrings #-}

module Mud.Misc.CurryTime where

import Data.Time (UTCTime(..), diffUTCTime, fromGregorian, getCurrentTime)
import Prelude hiding (min)


type Year  = Integer
type Month = Integer
type Week  = Integer
type Day   = Integer
type Hour  = Integer
type Min   = Integer
type Sec   = Integer


-- Months
curryMonthsInYear :: Month
curryMonthsInYear = 8


-- Weeks
curryWeeksInMonth :: Week
curryWeeksInMonth = 3


curryWeeksInYear :: Week
curryWeeksInYear = curryWeeksInMonth * curryMonthsInYear


-- Days
curryDaysInWeek :: Day
curryDaysInWeek = 7


curryDaysInMonth :: Day
curryDaysInMonth = curryDaysInWeek * curryWeeksInMonth


curryDaysInYear :: Day
curryDaysInYear = curryDaysInMonth * curryMonthsInYear


-- Hours
curryHoursInDay :: Hour
curryHoursInDay = 20


curryHoursInWeek :: Hour
curryHoursInWeek = curryHoursInDay * curryDaysInWeek


curryHoursInMonth :: Hour
curryHoursInMonth = curryHoursInWeek * curryWeeksInMonth


curryHoursInYear :: Hour
curryHoursInYear = curryHoursInMonth * curryMonthsInYear


-- Mins
curryMinsInHour :: Min
curryMinsInHour = 60


curryMinsInDay :: Min
curryMinsInDay = curryMinsInHour * curryHoursInDay


curryMinsInWeek :: Min
curryMinsInWeek = curryMinsInDay * curryDaysInWeek


curryMinsInMonth :: Min
curryMinsInMonth = curryMinsInWeek * curryWeeksInMonth


curryMinsInYear :: Min
curryMinsInYear = curryMinsInMonth * curryMonthsInYear


-- Seconds
currySecsInMin :: Sec
currySecsInMin = 60


currySecsInHour :: Sec
currySecsInHour = currySecsInMin * curryMinsInHour


currySecsInDay :: Sec
currySecsInDay = currySecsInHour * curryHoursInDay


currySecsInWeek :: Sec
currySecsInWeek = currySecsInDay * curryDaysInWeek


currySecsInMonth :: Sec
currySecsInMonth = currySecsInWeek * curryWeeksInMonth


currySecsInYear :: Sec
currySecsInYear = currySecsInMonth * curryMonthsInYear


-- ==================================================


data CurryTime = CurryTime { curryYear       :: Year
                           , curryMonth      :: Month
                           , curryWeek       :: Week
                           , curryDayOfMonth :: Day
                           , curryDayOfWeek  :: Day
                           , curryHour       :: Hour
                           , curryMin        :: Min
                           , currySec        :: Sec }


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
                        year       = succ $ years + 200
                        month      = succ $ months `rem` curryMonthsInYear
                        week       = succ $ weeks  `rem` curryWeeksInMonth
                        dayOfMonth = succ $ days   `rem` curryDaysInMonth
                        dayOfWeek  = succ $ days   `rem` curryDaysInWeek
                        hour       = succ $ hours  `rem` curryHoursInDay
                        min        = succ $ mins   `rem` curryMinsInHour
                        sec        = succ $ secs   `rem` currySecsInMin
                    in CurryTime year month week dayOfMonth dayOfWeek hour min sec
