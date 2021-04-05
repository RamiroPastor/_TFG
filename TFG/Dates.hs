module Dates where

data Month =
   January  |  February  |  March  |  April  |  May  |  June  |
   July  |  August  |  September  |  October  |  November  |  December
   deriving (Show, Enum)


showDay :: Int -> String
showDay n
   | n == 1   = "1st"
   | n == 2   = "2nd"
   | n == 3   = "3rd"
   |otherwise = (show n) ++ "th"


monthByNum :: Int -> Month
monthByNum = toEnum

showDate :: (Integer,Int,Int) -> String
showDate (yy,mm,dd) =
  "Madrid, " ++ (show $ monthByNum (mm-1)) ++ " " ++ (showDay dd) ++ " of " ++ show yy
