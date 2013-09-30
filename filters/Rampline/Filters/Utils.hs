-----------------------------------------------------------------------------
-- |
-- Module      :  Filters.Utils
-- Copyright   :  (c) Rino Jose 2013
-- License     :  BSD-style
--
-- Maintainer  :  @rjose
-- Stability   :  experimental
-- Portability :  portable
--
-- Provides utility functions for filters.
--
-----------------------------------------------------------------------------
module Filters.Utils where

import Data.List
import Data.Maybe
import Data.Time.Calendar
import Data.Time.Format
import System.Locale


--------------------------------------------------------------------------------
-- Standard date format for all filters.
--
dateFormat :: String
dateFormat = "%b %e, %Y"


--------------------------------------------------------------------------------
-- Formats day as string.
--
dayToString :: Day -> String
dayToString d = formatTime defaultTimeLocale dateFormat d


--------------------------------------------------------------------------------
-- Converts string to day.
--
--      NOTE: If the conversion should fail, we bail.
--
stringToDay :: String -> Day
stringToDay s = fromJust $ parseTime defaultTimeLocale dateFormat s


--------------------------------------------------------------------------------
-- Gets a list of dates from a start and end date (inclusive).
--
getDays :: Day -> Day -> [Day]
getDays start end
        | start > end = []
        | otherwise = start:getDays (addDays 1 start) end


--------------------------------------------------------------------------------
-- Joins Strings together.
--
joinWith :: String -> [String] -> String
joinWith _ [] = []
joinWith _ [w] = w
joinWith c (w:ws) = w ++ c ++ (joinWith c ws)


--------------------------------------------------------------------------------
-- Adds tab to beginning of String.
--
addTab :: String -> String
addTab s = "\t" ++ s
