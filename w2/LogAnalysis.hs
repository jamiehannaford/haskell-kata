{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log
import Data.Char

toDigits :: String -> Int
toDigits []     = 0
toDigits (x:xs) = (digitToInt(x) * 10 ^ length (xs)) + (toDigits xs) 

parseMessage :: String -> LogMessage
parseMessage s
	| s == ""   = Unknown s
	| otherwise = LogMessage msgType time message
	where msgType = case (words s) of
		("I":_)    -> Info
		("E":rest) -> (Error ( toDigits $ head $ words rest ))
		_          -> Warning
	      time    = 12345
	      message = "foo"
