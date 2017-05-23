module MudTests.Misc.CurryTime where

import Mud.Data.Misc
import Mud.Misc.CurryTime

import Test.Tasty.HUnit ((@?=), Assertion)


ct :: CurryTime
ct = CurryTime initCurryYear 1 1 1 1 0 0 0


-----


test_secsToCurryTime_zeroSecs :: Assertion
test_secsToCurryTime_zeroSecs = actual @?= expected
  where
    actual   = secsToCurryTime 0
    expected = ct


-----


test_secsToCurryTime_oneSec :: Assertion
test_secsToCurryTime_oneSec = actual @?= expected
  where
    actual   = secsToCurryTime 1
    expected = ct { currySec = 1 }


test_secsToCurryTime_oneMin :: Assertion
test_secsToCurryTime_oneMin = actual @?= expected
  where
    actual   = secsToCurryTime currySecsInMin
    expected = ct { curryMin = 1 }


test_secsToCurryTime_oneHour :: Assertion
test_secsToCurryTime_oneHour = actual @?= expected
  where
    actual   = secsToCurryTime currySecsInHour
    expected = ct { curryHour = 1 }


test_secsToCurryTime_oneDay :: Assertion
test_secsToCurryTime_oneDay = actual @?= expected
  where
    actual   = secsToCurryTime currySecsInDay
    expected = ct { curryDayOfWeek = 2, curryDayOfMonth = 2 }


test_secsToCurryTime_oneWeek :: Assertion
test_secsToCurryTime_oneWeek = actual @?= expected
  where
    actual   = secsToCurryTime currySecsInWeek
    expected = ct { curryDayOfMonth = 8, curryWeek = 2 }


test_secsToCurryTime_oneMonth :: Assertion
test_secsToCurryTime_oneMonth = actual @?= expected
  where
    actual   = secsToCurryTime currySecsInMonth
    expected = ct { curryMonth = 2 }


test_secsToCurryTime_oneYear :: Assertion
test_secsToCurryTime_oneYear = actual @?= expected
  where
    actual   = secsToCurryTime currySecsInYear
    expected = ct { curryYear = initCurryYear + 1 }


-----


test_secsToCurryTime_twoSecs :: Assertion
test_secsToCurryTime_twoSecs = actual @?= expected
  where
    actual   = secsToCurryTime 2
    expected = ct { currySec = 2 }


test_secsToCurryTime_twoMins :: Assertion
test_secsToCurryTime_twoMins = actual @?= expected
  where
    actual   = secsToCurryTime $ currySecsInMin * 2
    expected = ct { curryMin = 2 }


test_secsToCurryTime_twoHours :: Assertion
test_secsToCurryTime_twoHours = actual @?= expected
  where
    actual   = secsToCurryTime $ currySecsInHour * 2
    expected = ct { curryHour = 2 }


test_secsToCurryTime_twoDays :: Assertion
test_secsToCurryTime_twoDays = actual @?= expected
  where
    actual   = secsToCurryTime $ currySecsInDay * 2
    expected = ct { curryDayOfWeek = 3, curryDayOfMonth = 3 }


test_secsToCurryTime_twoWeeks :: Assertion
test_secsToCurryTime_twoWeeks = actual @?= expected
  where
    actual   = secsToCurryTime $ currySecsInWeek * 2
    expected = ct { curryDayOfMonth = 15, curryWeek = 3 }


test_secsToCurryTime_twoMonths :: Assertion
test_secsToCurryTime_twoMonths = actual @?= expected
  where
    actual   = secsToCurryTime $ currySecsInMonth * 2
    expected = ct { curryMonth = 3 }


test_secsToCurryTime_twoYears :: Assertion
test_secsToCurryTime_twoYears = actual @?= expected
  where
    actual   = secsToCurryTime $ currySecsInYear * 2
    expected = ct { curryYear = initCurryYear + 2 }


-----


test_secsToCurryTime_oneMinOneSec :: Assertion
test_secsToCurryTime_oneMinOneSec = actual @?= expected
  where
    actual   = secsToCurryTime $ currySecsInMin + 1
    expected = ct { currySec = 1, curryMin = 1 }


test_secsToCurryTime_oneHourOneSec :: Assertion
test_secsToCurryTime_oneHourOneSec = actual @?= expected
  where
    actual   = secsToCurryTime $ currySecsInHour + 1
    expected = ct { currySec = 1, curryHour = 1 }


test_secsToCurryTime_oneDayOneSec :: Assertion
test_secsToCurryTime_oneDayOneSec = actual @?= expected
  where
    actual   = secsToCurryTime $ currySecsInDay + 1
    expected = ct { currySec = 1, curryDayOfWeek = 2, curryDayOfMonth = 2 }


test_secsToCurryTime_oneWeekOneSec :: Assertion
test_secsToCurryTime_oneWeekOneSec = actual @?= expected
  where
    actual   = secsToCurryTime $ currySecsInWeek + 1
    expected = ct { currySec = 1, curryDayOfMonth = 8, curryWeek = 2 }


test_secsToCurryTime_oneMonthOneSec :: Assertion
test_secsToCurryTime_oneMonthOneSec = actual @?= expected
  where
    actual   = secsToCurryTime $ currySecsInMonth + 1
    expected = ct { currySec = 1, curryMonth = 2 }


test_secsToCurryTime_oneYearOneSec :: Assertion
test_secsToCurryTime_oneYearOneSec = actual @?= expected
  where
    actual   = secsToCurryTime $ currySecsInYear + 1
    expected = ct { currySec = 1, curryYear = initCurryYear + 1 }


-----


test_secsToCurryTime_sevenDays :: Assertion
test_secsToCurryTime_sevenDays = actual @?= expected
  where
    actual   = secsToCurryTime $ currySecsInDay * 7
    expected = ct { curryDayOfMonth = 8, curryWeek = 2 }


test_secsToCurryTime_eightDays :: Assertion
test_secsToCurryTime_eightDays = actual @?= expected
  where
    actual   = secsToCurryTime $ currySecsInDay * 8
    expected = ct { curryDayOfWeek = 2, curryDayOfMonth = 9, curryWeek = 2 }


test_secsToCurryTime_fourteenDays :: Assertion
test_secsToCurryTime_fourteenDays = actual @?= expected
  where
    actual   = secsToCurryTime $ currySecsInDay * 14
    expected = ct { curryDayOfMonth = 15, curryWeek = 3 }


test_secsToCurryTime_fifteenDays :: Assertion
test_secsToCurryTime_fifteenDays = actual @?= expected
  where
    actual   = secsToCurryTime $ currySecsInDay * 15
    expected = ct { curryDayOfWeek = 2, curryDayOfMonth = 16, curryWeek = 3 }


test_secsToCurryTime_twentyOneDays :: Assertion
test_secsToCurryTime_twentyOneDays = actual @?= expected
  where
    actual   = secsToCurryTime $ currySecsInDay * 21
    expected = ct { curryMonth = 2 }


test_secsToCurryTime_twentyTwoDays :: Assertion
test_secsToCurryTime_twentyTwoDays = actual @?= expected
  where
    actual   = secsToCurryTime $ currySecsInDay * 22
    expected = ct { curryDayOfWeek = 2, curryDayOfMonth = 2, curryMonth = 2 }


-----


test_secsToCurryTime_oneSecLessThanAYear :: Assertion
test_secsToCurryTime_oneSecLessThanAYear = actual @?= expected
  where
    actual   = secsToCurryTime . sum $ [ currySecsInMonth * 7
                                       , currySecsInDay   * 20
                                       , currySecsInHour  * 19
                                       , currySecsInMin   * 59
                                       , 59 ]
    expected = ct { currySec        = 59
                  , curryMin        = 59
                  , curryHour       = 19
                  , curryDayOfWeek  = 7
                  , curryDayOfMonth = 21
                  , curryWeek       = 3
                  , curryMonth      = 8 }


test_secsToCurryTime_oneSecLessThanAYearPlusOne :: Assertion
test_secsToCurryTime_oneSecLessThanAYearPlusOne = actual @?= expected
  where
    actual   = secsToCurryTime . sum $ [ currySecsInMonth * 7
                                       , currySecsInDay   * 20
                                       , currySecsInHour  * 19
                                       , currySecsInMin   * 59
                                       , 59
                                       , 1 ]
    expected = ct { curryYear = initCurryYear + 1 }


test_secsToCurryTime_oneSecLessThanAYearPlusTwo :: Assertion
test_secsToCurryTime_oneSecLessThanAYearPlusTwo = actual @?= expected
  where
    actual   = secsToCurryTime . sum $ [ currySecsInMonth * 7
                                       , currySecsInDay   * 20
                                       , currySecsInHour  * 19
                                       , currySecsInMin   * 59
                                       , 59
                                       , 2 ]
    expected = ct { currySec = 1, curryYear = initCurryYear + 1 }


test_secsToCurryTime_oneSecLessThanTwoYears :: Assertion
test_secsToCurryTime_oneSecLessThanTwoYears = actual @?= expected
  where
    actual   = secsToCurryTime $ sum [ currySecsInMonth * 7
                                     , currySecsInDay   * 20
                                     , currySecsInHour  * 19
                                     , currySecsInMin   * 59
                                     , 59 ] * 2 + 1
    expected = ct { currySec        = 59
                  , curryMin        = 59
                  , curryHour       = 19
                  , curryDayOfWeek  = 7
                  , curryDayOfMonth = 21
                  , curryWeek       = 3
                  , curryMonth      = 8
                  , curryYear       = initCurryYear + 1 }


test_secsToCurryTime_oneSecLessThanTwoYearsPlusOne :: Assertion
test_secsToCurryTime_oneSecLessThanTwoYearsPlusOne = actual @?= expected
  where
    actual   = secsToCurryTime $ sum [ currySecsInMonth * 7
                                     , currySecsInDay   * 20
                                     , currySecsInHour  * 19
                                     , currySecsInMin   * 59
                                     , 59 ] * 2 + 1 + 1
    expected = ct { curryYear = initCurryYear + 2 }


test_secsToCurryTime_oneSecLessThanTwoYearsPlusTwo :: Assertion
test_secsToCurryTime_oneSecLessThanTwoYearsPlusTwo = actual @?= expected
  where
    actual   = secsToCurryTime $ sum [ currySecsInMonth * 7
                                     , currySecsInDay   * 20
                                     , currySecsInHour  * 19
                                     , currySecsInMin   * 59
                                     , 59 ] * 2 + 1 + 2
    expected = ct { currySec = 1, curryYear = initCurryYear + 2 }
