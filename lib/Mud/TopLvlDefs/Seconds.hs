{-# LANGUAGE OverloadedStrings #-}

module Mud.TopLvlDefs.Seconds where


type Seconds = Int -- 10^6 microseconds (the unit used by "threadDelay").


type Milliseconds = Int -- 10^-3 or 1/1000 or 0.001 seconds.


fifteenMinsInSecs :: Seconds
fifteenMinsInSecs = 15 {- mins -} * 60 {- secs -}


fiveMinsInSecs :: Seconds
fiveMinsInSecs = 5 {- mins -} * 60 {- secs -}


oneDayInSecs :: Seconds
oneDayInSecs = 1 {- day -} * 24 {- hrs -} * 60 {- mins -} * 60 {- secs -}


oneHrInSecs :: Seconds
oneHrInSecs = 1 {- hr -} * 60 {- mins -} * 60 {- secs -}


oneMinInSecs :: Seconds
oneMinInSecs = 60 {- secs -}


sixHrsInSecs :: Seconds
sixHrsInSecs = 6 {- hrs -} * 60 {- mins -} * 60 {- secs -}


tenMinsInSecs :: Seconds
tenMinsInSecs = 10 {- mins -} * 60 {- secs -}


threeHrsInSecs :: Seconds
threeHrsInSecs = 3 {- hrs -} * 60 {- mins -} * 60 {- secs -}


twoMinsInSecs :: Seconds
twoMinsInSecs = 2 {- mins -} * 60 {- secs -}
