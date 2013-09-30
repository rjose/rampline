-----------------------------------------------------------------------------
-- |
-- Module      :  Work
-- Copyright   :  (c) Rino Jose 2013
-- License     :  BSD-style
--
-- Maintainer  :  @rjose
-- Stability   :  experimental
-- Portability :  portable
--
-- Provides parsing of work items.
--
-----------------------------------------------------------------------------

module Work(
        Triage(..),
        Work(..),
        workFromString,
        formatEstimate,
        module SkillAmount
) where

import Data.List
import Data.List.Split
import Data.Maybe

import SkillAmount


-- =============================================================================
-- Data types
--
type Id = String
type Estimate = SkillAmount
type Value = Float

--------------------------------------------------------------------------------
-- Enumerates triage levels
--
data Triage
        = P1 | P1_5 | P2 | P2_5 | P3
        deriving (Eq, Ord, Enum)

instance Show Triage where
        show P1 = "1"
        show P1_5 = "1.5"
        show P2 = "2"
        show P2_5 = "2.5"
        show P3 = "3"

--------------------------------------------------------------------------------
-- | Captures work fields necessary for answering quarterly planning questions.
--
--      The ids should be generated outside of this filter. The "WorkNone"
--      constructor is used to capture an empty work item.
--
data Work
        = Work { id :: Id,
                 rank :: Int,
                 name :: String,
                 estimate :: [Estimate],
                 triage :: Triage,
                 track :: String,
                 team :: String,
                 value :: Value,
                 prereqs :: [Id]
               }
        | WorkNone
               deriving (Show)


-- =============================================================================
-- Public API
--

formatEstimate :: [Estimate] -> String
formatEstimate est = intercalate ", " $ map show est

--------------------------------------------------------------------------------
-- | Constructs a work item from an input string.
--
--      The input strings will typically come from data cat'd into the qplan
--      filter from other filters.
--
workFromString :: String -> Work
workFromString s = Work id rank name estimate triage track team value prereqs
        where
                vals = splitOn "\t" s
                id = vals !! 0
                rank = read $ vals !! 1
                name = vals !! 2
                estimate_str = vals !! 3
                triage_str = vals !! 4
                track = vals !! 5
                team = vals !! 6
                value_str = vals !! 7
                prereqs = splitOn "," $ vals !! 8
                value = if value_str == "" then 0 else read value_str
                estimate = fromVectorString estimate_str
                triage = parseTriage triage_str


-- =============================================================================
-- Internal functions
--

--------------------------------------------------------------------------------
-- Converts string into a triage value.
--
parseTriage :: String -> Triage
parseTriage "" = P3
parseTriage s = valTriage (read s :: Float)
        where
                valTriage val
                        | val <= 1   = P1
                        | val <= 1.5 = P1_5
                        | val <= 2   = P2
                        | val <= 2.5 = P2_5
                        | otherwise  = P3
