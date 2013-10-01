-----------------------------------------------------------------------------
-- |
-- Module      :  Filters.RamplineApp
-- Copyright   :  (c) Rino Jose 2013
-- License     :  BSD-style
--
-- Maintainer  :  @rjose
-- Stability   :  experimental
-- Portability :  portable
--
-- Converts work and staff streams into JSON data appropriate for Rampline app.
-- This enables analysis of staff skill shortages by triage.
--
-----------------------------------------------------------------------------
module Filters.RamplineApp (filterString) where

import Control.Applicative
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Time.Calendar
import Data.Time.Format
import qualified Data.Set as Set
import System.Locale

import Filters.Schedule.Internal
import Filters.Utils
import Person
import SkillAmount
import StackStream
import Work

-- =============================================================================
-- Data types
--
type SkillName = String
type TrackName = String
type ProductName = String
type ThemeName = String

type TrackWork =  [[Work]] -- List of tracks, each with a list of work
type TrackStaff = [[[Person]]] -- List of tracks, each with a list of skill teams
type TrackManpower = [[Float]] -- List of tracks, each with list of skills manpower
type TrackDemand = [[[Float]]] -- Tracks, each with a triage list, each with skills manpower
type TrackAvail = TrackDemand -- Same size and shape as TrackDemand
type TrackFeasibility = [[Bool]] -- List of tracks, each with bool list corresp. to worklist
type WorkDay = (Day, Bool) -- Day and True if workday
type TrackStaffAvail = [SkillAvailabilities] -- List of skill groups for each track

data Params = Params { startDate :: Day,
                       endDate :: Day,
                       schedSkills :: [String]
                     }
                     deriving (Show)

-- =============================================================================
-- Public API
--

--------------------------------------------------------------------------------
-- | Converts rampline input streams into result streams for rampline.
--
--
filterString :: String -> String
filterString s = if any isNothing [workStream, staffStream, holidayStream, paramStream]
                        then error "One of the input streams was missing"
                        else result
        where
                -- Unstack the input streams
                streams = unstack $ lines s
                workStream = find (("rampline work v1" ==) . header) streams
                staffStream = find (("rampline staff v1" ==) . header) streams
                holidayStream = find (("rampline holidays v1" ==) . header) streams
                paramStream = find (("rampline params v1" ==) . header) streams

                -- Parse info out of streams
                params = getParams $ fromJust paramStream
                workItems = map workFromString $ content $ fromJust workStream
                staff = sort $ map personFromString $ content $ fromJust staffStream

                -- Define "master lists"
                --      Any list organized by track, skill, or time will
                --      correspond directly to these lists.
                tracks = getTracks staff workItems
                products = getProducts workItems
                themes = getThemes workItems
                skills = getSkills staff workItems
                days = getDays (startDate params) (endDate params)

                -- Group work and staff into tracks
                trackWork = workByTrack tracks workItems
                trackStaff = staffByTrackSkills tracks skills staff

                -- Schedule work based on track assignments
                workDays = getWorkdays days (fromJust holidayStream)
                trackStaffAvail = getTrackStaffAvail workDays trackStaff
                trackDates = estimateEndDates (schedSkills params)
                                              trackWork days trackStaffAvail
                months = getMonths trackDates

                -- Generate result
                productStream = Stream "rampline products v1" products
                themeStream = Stream "rampline themes v1" themes
                monthStream = Stream "rampline months v1" months
                trackWork' = zip trackWork trackDates
                trackWorkStream = Stream "rampline track work v1" $
                        stack (map getWorkStream trackWork')

                result = unlines $ stack [fromJust paramStream,
                                          productStream, themeStream, monthStream
                                         --       , trackWorkStream
                                         ]


-- =============================================================================
-- Internal functions -- Grouping functions
--

getMonths :: [[Maybe Day]] -> [String]
getMonths trackDates = result
        where
                uniqueDates = sort $ map fromJust $ filter isJust $ foldr union [] trackDates
                months = map dayToString2 uniqueDates
                result = map (!! 0) $ group months

getProducts :: [Work] -> [ProductName]
getProducts workItems = result
        where
               productSet = Set.fromList $ map Work.product workItems
               result = sort $ Set.toList productSet


getThemes :: [Work] -> [ThemeName]
getThemes workItems = result
        where
               themeSet = Set.fromList $ map Work.theme workItems
               result = sort $ Set.toList themeSet

--------------------------------------------------------------------------------
-- Takes union of tracks from a list of people and work items.
--
--      NOTE: This also prepends the "All" track to represent items from all
--      tracks.
--
getTracks :: [Person] -> [Work] -> [TrackName]
getTracks staff workItems = result
        where
                workTracks = Set.fromList $ map Work.track workItems
                staffTracks = Set.fromList $ map Person.track staff
                result' = Set.toList $ Set.union workTracks staffTracks
                result = "All":(sort result')

--------------------------------------------------------------------------------
-- Takes union of skills from a list of people and work items.
--
getSkills :: [Person] -> [Work] -> [SkillName]
getSkills staff workItems = result
        where
                estimates = concat $ map Work.estimate workItems
                skillsDemanded = Set.fromList $ map skill' estimates
                skillsAvailable = Set.fromList $ map Person.skill staff
                result' = Set.toList $ Set.union skillsDemanded skillsAvailable
                result = sort $ filter (\s -> s /= "") result'


--------------------------------------------------------------------------------
-- Groups work items by specified track names.
--
--      NOTE: All of the work items "ws" are prepended to the result. This
--      corresponds to the "All" track.
--
workByTrack :: [TrackName] -> [Work] -> TrackWork
workByTrack (all:tracks) ws = result
        where
                result' = (\t -> filter (\w -> t == Work.track w) ws) <$> tracks
                result = map (sortBy (\l r -> rank l `compare` rank r)) $ ws:result'


--------------------------------------------------------------------------------
-- Organizes staff by track and then by skill within each track.
--
--      The track names and skills in question are provided as arguments. In
--      effect, this allows us to assume the order of items in lists is
--      meaningful (e.g., the first item here corresponds to the first track).
--
staffByTrackSkills :: [TrackName] -> [SkillName] -> [Person] -> TrackStaff
staffByTrackSkills (all:tracks) skills staff = result
        where
          staffByTrack' = map (\t -> filter (\p -> t == Person.track p) staff) tracks
          staffByTrack = staff:staffByTrack'
          groupBySkills ps = map (\s -> filter (\p -> s == Person.skill p) ps) skills
          result = map groupBySkills staffByTrack



-- =============================================================================
-- Internal functions -- Supply and Demand
--


--------------------------------------------------------------------------------
-- Maps an empty demand array into one with a specified number of zeroes.
--
conditionDemand :: Int -> [Float] -> [Float]
conditionDemand len demand = result
        where
                result = if demand == []
                         then take len $ repeat 0
                         else demand

--------------------------------------------------------------------------------
-- Accumulates manpower totals.
--
accManpower :: [[Float]] -> [[Float]]
accManpower [] = []
accManpower (m:ms) = scanl (\acc mp -> sumManpower [acc, mp]) m ms


--------------------------------------------------------------------------------
-- Sums list of manpower items.
--
--      NOTE: The position in the manpower arrays corresponds to whatever skills
--      array was used in the calling functions.
--
sumManpower :: [[Float]] -> [Float]
sumManpower [] = []
sumManpower (m:ms) = result
        where
                result = foldl (\acc mp -> zipWith (+) acc mp) m ms

-- =============================================================================
-- Internal functions -- Work scheduling
--

--------------------------------------------------------------------------------
-- Returns WorkDays given days and a holiday stream.
--
getWorkdays :: [Day] -> Stream -> [WorkDay]
getWorkdays days holidayStream = map (isWorkDay holidays) days
        where
                holidays = map stringToDay $ content holidayStream


--------------------------------------------------------------------------------
-- Estimates dev complete dates for work in each track.
--
--      This works by constructing schedule availability information for each
--      skill within each track and laying work out in order. The first track is
--      the "All" track which contains all work and all staff across all tracks.
--      The dev complete dates in the "All" track assume that all staff is
--      available to work on any work item. Thus, there are two estimates for
--      the dev complete date for an item: one where track boundaries are
--      respected, and one where they are not.
--
--      NOTE: The Params contains a schedSkills field which is used to schedule
--      work items against skill sets. This defines skills that are required for
--      development *not* QA or any other skill type.
--
estimateEndDates :: [SkillName] -> TrackWork -> [Day] -> [SkillAvailabilities] ->
                     [[Maybe Day]]
estimateEndDates skills trackWork days trackStaffAvail = result
        where
                schedAvails = map (\avail -> (days, avail)) trackStaffAvail
                result = zipWith (schedule skills) trackWork schedAvails


--------------------------------------------------------------------------------
-- Sums availability of staff for each track and skill group.
--
--      NOTE: This does not take individuals' vacations into account. That is
--      done by getManpower.
--
getTrackStaffAvail :: [WorkDay] -> TrackStaff -> TrackStaffAvail
getTrackStaffAvail workdays trackStaff = result
        where
                result = [map getAvail skillGroups | skillGroups <- trackStaff,
                                let
                                getAvail = sumAvailability . map (getAvailability workdays)]



-- =============================================================================
-- Internal functions -- Output stream generation
--

--------------------------------------------------------------------------------
-- Constructs stream of work data.
--
getWorkStream :: ([Work], [Maybe Day]) -> Stream
getWorkStream (ws, ds) = result
        where
                result = Stream "rampline track item" (zipWith format ws ds)
                format w d = joinWith "\t" [Work.track w,
                                              show $ rank w,
                                              show $ triage w,
                                              Work.name w,
                                              formatEstimate $ estimate w,
                                              formatDay d]
                formatDay d = if isNothing d
                                then "DNF"
                                else dayToString $ fromJust d


--------------------------------------------------------------------------------
-- Constructs stream of tracks.
--
--      Every list of items by tracks corresponds to these tracks in the same
--      order.
--
getTrackGroupStream :: [[Person]] -> Stream
getTrackGroupStream skillGroup = result
        where
                result = Stream "rampline track item" $ stack (map getSkillStream skillGroup)


--------------------------------------------------------------------------------
-- Constructs stream of skills.
--
--      Every list of items by skills corresponds to these skills in the same
--      order.
--
getSkillStream :: [Person] -> Stream
getSkillStream people = result
        where
                result = Stream "qplan skill item" (map Person.name people)


--------------------------------------------------------------------------------
-- Constructs stream of triages.
--
getParams :: Stream -> Params
getParams (Stream _ ls) = Params startDate endDate schedSkills
        where
                params = splitOn "\t" (head ls)
                startDate = stringToDay $ params !! 0
                endDate = stringToDay $ params !! 1
                schedSkills = splitOn ":" $ params !! 2


-- =============================================================================
-- Internal functions -- Development/Debug support
--

test = do
        content <- readFile "rampline_cond.txt"
        let result = filterString content
        putStr result
