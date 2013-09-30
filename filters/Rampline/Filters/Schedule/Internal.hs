-----------------------------------------------------------------------------
-- |
-- Module      :  Filters.Schedule.Internal
-- Copyright   :  (c) Rino Jose 2013
-- License     :  BSD-style
--
-- Maintainer  :  @rjose
-- Stability   :  experimental
-- Portability :  portable
--
-- Implements scheduling algorithms.
--
-----------------------------------------------------------------------------
module Filters.Schedule.Internal where

import Data.List
import Data.Maybe
import Data.Time.Calendar
import Data.Time.Format
import System.Locale
import Control.Monad.State

import Filters.Utils
import Person
import Work

-- =============================================================================
-- Data types
--
type Availability = [Float] -- List corresponding to num people availabile on a given day
type SkillAvailabilities = [Availability] -- Used to group availabilities into skill groups
type SchedAvail = ([Day], SkillAvailabilities) -- Schedule state for State Monad


-- =============================================================================
-- Scheduling functions
--

--------------------------------------------------------------------------------
-- Schedules a list of Work items.
--
--      If there wasn't enough time to schedule a work item, then its dev
--      complete is Nothing.
--
--      NOTE: This uses State Monad to manage the SchedAvail state.
--
schedule :: [String] -> [Work] -> SchedAvail -> [Maybe Day]
schedule skills work schedAvail = result
        where
                (result, _) = runState (sched work) schedAvail
                sched ws = do
                        ds' <- mapM (state . scheduleWork skills) ws
                        let ds = zipWith (\d w -> if (estimate w == [SkillNone])
                                                    then Nothing
                                                    else d) ds' work
                        return ds


--------------------------------------------------------------------------------
-- Implements stateful scheduling computation.
--
--      This is chained together with other scheduling algo calls to schedule
--      work items. This is an instance of the State Monad.
--
scheduleWork :: [String] -> Work -> SchedAvail -> (Maybe Day, SchedAvail)
scheduleWork skills work (days, skillAvails) = result
        where
                reqStaff = [(maxPerSlot, 5.0 * numWeeks * maxPerSlot) |
                            (maxPerSlot, numWeeks) <- getRequiredStaff skills work]

                result' = zipWith (\a r -> consume a [] 0 r) skillAvails reqStaff

                endDateIndexes = map fst result'
                endDate = if any isNothing endDateIndexes
                            then Nothing 
                            else fmap (days !!) (maximum endDateIndexes)

                result = (endDate, (days, map snd result'))


--------------------------------------------------------------------------------
-- Implements core scheduling logic.
--
--      This is a recursive function that lays work out for a specific skill. It
--      marches down the list of skill availailabilities, reducing both the
--      supply for a day and its demand.
--
--      There is a maximum amount that can be consumed in a day. This
--      corresponds to the scalar part of the estimate. For instance, an
--      estimate of "3M" means that 3 people can work on this task at the same
--      time.
--
consume :: [Float] -> [Float] -> Int -> (Float, Float) -> (Maybe Int, [Float])
consume avail newAvail startIndex (maxPerSlot, numDays)
        | numDays > 0 && avail == [] = (Nothing, newAvail ++ avail)
        | numDays <= 0 && newAvail == [] = (Just startIndex, newAvail ++ avail)
        | numDays <= 0 && newAvail /= [] = (Just (startIndex - 1), newAvail ++ avail)
        | otherwise = consume (tail avail) (newAvail ++ [h])
                              (startIndex + 1) (maxPerSlot, numDays - amt)
        where
                amt = minimum [(head avail), maxPerSlot, numDays] 
                h = (head avail) - amt


-- =============================================================================
-- Misc Support
--

--------------------------------------------------------------------------------
-- Sums availabilities.
--
sumAvailability :: [Availability] -> Availability
sumAvailability [] = []
sumAvailability (a:as) = foldr (zipWith (+)) a as


--------------------------------------------------------------------------------
-- Returns a person's availability given team work days.
--
getAvailability :: [(Day, Bool)] -> Person -> Availability
getAvailability workdays person = result
        where
                unavailDays = vacation person
                avail uds wd = if or [(fst wd) `elem` uds, snd wd == False]
                                 then 0
                                 else 1
                result = map (avail unavailDays) workdays


--------------------------------------------------------------------------------
-- Given a set of holidays and a day, returns if day is a workday
--
isWorkDay :: [Day] -> Day -> (Day, Bool)
isWorkDay holidays day = result
        where
                dayOfWeek = formatTime defaultTimeLocale "%a" day
                weekdays = ["Sat", "Sun"]
                result = if or [(dayOfWeek `elem` weekdays), (day `elem` holidays)]
                            then (day, False)
                            else (day, True)


-- =============================================================================
-- Supply and Demand
--

--------------------------------------------------------------------------------
-- Returns work demand given a list of skills.
--
--      This returns a subset of the demand for a work item.
--
getRequiredStaff :: [String] -> Work -> [(NumStaff, NumWeeks)]
getRequiredStaff skills work = result
        where
                result = [reqStaff | s <- skills,
                           let estimates = estimate work
                               reqStaff' = find (\e -> s == skill' e) estimates
                               reqStaff = if isNothing reqStaff'
                                          then (0, 0)
                                          else requiredStaff $ fromJust reqStaff']

--------------------------------------------------------------------------------
-- Returns required manpower in terms of specified skills.
--
--      The [String] list has a list of the skills that correspond to the
--      returned list of Floats.
--
getWorkManpower :: [String] -> Work -> [Float]
getWorkManpower skills work = result
        where
                result = [manpower | s <- skills,
                           let estimates = estimate work
                               manpower' = find (\e -> s == skill' e) estimates
                               manpower = if isNothing manpower'
                                          then 0
                                          else numval' $ fromJust manpower']
