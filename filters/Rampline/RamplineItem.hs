-----------------------------------------------------------------------------
-- |
-- Module      :  RamplineItem
-- Copyright   :  (c) Rino Jose 2013
-- License     :  BSD-style
--
-- Maintainer  :  @rjose
-- Stability   :  experimental
-- Portability :  portable
--
--
-----------------------------------------------------------------------------

module RamplineItem(
        RamplineItem(..)
) where

import Data.List
import Data.List.Split
import Data.Maybe
import Data.Time.Calendar

-- =============================================================================
-- Data types
--

type Id = String

data RamplineItem
        = RamplineItem { id :: Id,
                         team :: String,
                         product :: String,
                         theme :: String,
                         track :: String,
                         name :: String,
                         devDate :: Maybe Day
                       }
               deriving (Show)
