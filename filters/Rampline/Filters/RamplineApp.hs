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
-- | Converts qplan input streams into result streams for qplan.
--
--      This groups work items by triage and staff by skill area for all tracks
--      present in the input streams. It computes cumulative demand for
--      resources by triage level. This also estimates when resources will be
--      exhausted assuming work items are staffed in the order they appear in
--      the streams -- this is used to estimate feasibilty of work.  This also
--      computes staff availability by skill group in each track and uses this
--      to estimate dev complete dates for each work item.
--
--      The final output is a set of stacked streams with this information
--      presented in a way that is easy for other programs to parse.
--
filterString :: String -> String
filterString s = if any isNothing [workStream, staffStream, holidayStream, paramStream]
                        then error "One of the input streams was missing"
                        else result
        where
                -- Unstack the input streams
                streams = unstack $ lines s
                workStream = find (("qplan work v1" ==) . header) streams
                staffStream = find (("qplan staff v1" ==) . header) streams
                holidayStream = find (("qplan holidays v1" ==) . header) streams
                paramStream = find (("qplan params v1" ==) . header) streams

                -- Parse info out of streams
                params = getParams $ fromJust paramStream
                workItems = map workFromString $ content $ fromJust workStream
                staff = sort $ map personFromString $ content $ fromJust staffStream

                -- Define "master lists"
                --      Any list organized by track, skill, or time will
                --      correspond directly to these lists.
                tracks = getTracks staff workItems
                skills = getSkills staff workItems
                days = getDays (startDate params) (endDate params)
                triages = map show [P1 .. P3]

                -- Group work and staff into tracks
                trackWork = workByTrack tracks workItems
                trackStaff = staffByTrackSkills tracks skills staff

                -- Compute resource demand and feasibility
                workDays = getWorkdays days (fromJust holidayStream)
                trackStaffAvail = getTrackStaffAvail workDays trackStaff
                trackManpower = getManpower trackStaffAvail
                trackDemand = getTrackDemand trackWork skills
                trackFeasibility = getTrackFeasibility skills trackManpower trackWork

                -- Schedule work based on track assignments
                trackDates = estimateEndDates (schedSkills params)
                                              trackWork days trackStaffAvail

                -- Generate result
                trackStream = Stream "qplan tracks v1" tracks
                skillStream = Stream "qplan skills v1" skills
                triageStream = Stream "qplan triages v1" triages

                manpowerStream = Stream "qplan track manpower v1"
                        [l | mp <- trackManpower, let l = joinWith "\t" $ map show mp]

                trackDemandStream = Stream "qplan track-triage demand v1" $
                        stack (map getTriageStream trackDemand)

                trackStaffStream = Stream "qplan track-skill staff v1" $
                        stack (map getTrackGroupStream trackStaff)

                trackWork' = zip3 trackWork trackFeasibility trackDates
                trackWorkStream = Stream "qplan track work v1" $
                        stack (map getWorkStream trackWork')

                result = unlines $ stack [fromJust paramStream,
                                          trackStream, skillStream, triageStream,
                                          manpowerStream, trackDemandStream,
                                          trackStaffStream, trackWorkStream]


-- =============================================================================
-- Internal functions -- Grouping functions
--

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
-- Computes manpower for track staff.
--
getManpower :: TrackStaffAvail -> TrackManpower
getManpower avail = [map (foldl (\a x -> a + (x/5.0)) 0) skillGroups |
                     skillGroups <- avail]


--------------------------------------------------------------------------------
-- Computes manpower requirements for track work items.
--
--      Track work is divided into triage groups. The demand is reported as a
--      running total across the triage groups, from high to low priority.
--
--      The resulting manpower skill requirements will be returned in the same
--      order as the "skills" list. If a skill is not required for a work item,
--      0 will be returned as the skill requirement.
--
getTrackDemand :: TrackWork -> [SkillName] -> TrackDemand
getTrackDemand trackWork skills = result
        where
                result = [trackDemand | ws <- trackWork,
                           let triagedWork = map (selectTriage ws) triages
                               trackDemand' = map (map (getWorkManpower skills)) triagedWork
                               trackDemand'' = map sumManpower trackDemand'
                               trackDemand''' = map (conditionDemand len) trackDemand''
                               trackDemand = accManpower trackDemand'''
                         ]
                selectTriage work tri = filter (\w -> tri == triage w) work
                triages = [P1 .. P3]
                len = length skills


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


--------------------------------------------------------------------------------
-- Computes feasibility of track work.
--
--      This checks the net availability across all items in all tracks and
--      returns corresponding Bool values if any of the required skills for an
--      item have been exhausted.
--
getTrackFeasibility :: [SkillName] -> TrackManpower -> TrackWork -> TrackFeasibility
getTrackFeasibility skills manpower trackWork = result
        where
                netAvail mp ds = tail $ scanl (zipWith (-)) mp ds
                isFeasibile as ds = and $
                    zipWith (\a d -> if d > 0 && a < 0 then False else True) as ds
                trackDemand = [map (getWorkManpower skills) ws| ws <- trackWork]
                trackAvail = zipWith netAvail manpower trackDemand
                result = zipWith (zipWith isFeasibile) trackAvail trackDemand



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
getWorkStream :: ([Work], [Bool], [Maybe Day]) -> Stream
getWorkStream (ws, fs, ds) = result
        where
                result = Stream "qplan track item" (zipWith3 format ws fs ds)
                format w f d = joinWith "\t" [Work.track w,
                                              show $ rank w,
                                              show $ triage w,
                                              Work.name w,
                                              formatEstimate $ estimate w,
                                              show f,
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
                result = Stream "qplan track item" $ stack (map getSkillStream skillGroup)


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
--      Every list of items by triage corresponds to these triage items in the
--      same order.
--
getTriageStream :: TrackManpower -> Stream
getTriageStream manpower = result
        where
                result = Stream "qplan triage item"
                        [l | mp <- manpower, let l = joinWith "\t" $ map show mp]


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
        content <- readFile "_q4plan.txt"
        let result = filterString content
        putStr result
