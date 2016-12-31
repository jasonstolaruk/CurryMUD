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
currySecsInHour  = currySecsInMin   * curryMinsInHour
currySecsInDay   = currySecsInHour  * curryHoursInDay
currySecsInWeek  = currySecsInDay   * curryDaysInWeek
currySecsInMonth = currySecsInWeek  * curryWeeksInMonth
currySecsInYear  = currySecsInMonth * curryMonthsInYear


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
                        year       = years + 200
                        month      = months `rem` curryMonthsInYear
                        week       = weeks  `rem` curryWeeksInMonth
                        dayOfMonth = days   `rem` curryDaysInMonth
                        dayOfWeek  = days   `rem` curryDaysInWeek
                        hour       = hours  `rem` curryHoursInDay
                        min        = mins   `rem` curryMinsInHour
                        sec        = secs   `rem` currySecsInMin
                    in CurryTime year month week dayOfMonth dayOfWeek hour min sec
