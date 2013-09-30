-----------------------------------------------------------------------------
-- |
-- Module      :  Person
-- Copyright   :  (c) Rino Jose 2013
-- License     :  BSD-style
--
-- Maintainer  :  @rjose
-- Stability   :  experimental
-- Portability :  portable
--
-- Provides parsing of person items.
--
-----------------------------------------------------------------------------

module Person (
        Person(..),
        personFromString
) where

import Data.List.Split
import Data.Time.Calendar

import Filters.Utils


-- =============================================================================
-- Data types
--
type Id = String

data Person = Person { id :: Id,
                       name :: String,
                       team :: String,
                       track :: String,
                       skill :: String,
                       vacation :: [Day]
                     }
                     deriving (Show, Eq)

instance Ord Person where
        compare l r = compare (name l) (name r)


-- =============================================================================
-- Public API
--

--------------------------------------------------------------------------------
-- | Constructs Person from string.
--
personFromString :: String -> Person
personFromString s = Person id name team track skill vacation
        where
                vals = splitOn "\t" s
                id = vals !! 0
                name = vals !! 1
                team = vals !! 2
                track = vals !! 3
                skill = vals !! 4
                vacation' = splitOn ":" $ vals !! 5
                vacation = if vacation' == [""]
                                then []
                                else map stringToDay vacation'
