-----------------------------------------------------------------------------
-- |
-- Module      :  StackStream
-- Copyright   :  (c) Rino Jose 2013
-- License     :  BSD-style
-- 
-- Maintainer  :  @rjose
-- Stability   :  experimental
-- Portability :  portable
--
-- Provides ability to work with stacked streams to separate them into
-- component streams or to create stacked streams from different sources.
--
-----------------------------------------------------------------------------

-- =============================================================================
-- Module definition
--
module StackStream (
        stack,
        unstack,
        Stream(..)
) where


-- =============================================================================
-- Module imports
--
import Data.List
import Data.Maybe
import Control.Monad.State
import Control.Monad
import Filters.Utils


-- =============================================================================
-- Algebraic data types
--

type Header = String

--------------------------------------------------------------------------------
-- | Component of stacked stream.
--
data Stream
        = Stream {header :: Header, content :: [String]}
        | EmptyStream
        deriving (Show)



-- =============================================================================
-- Public API
--

stack :: [Stream] -> [String]
stack [] = []
stack ((Stream h c):ss) = concat [["=====" ++ h], (map addTab c), stack ss]


--------------------------------------------------------------------------------
-- | Translates a stacked stream into its components.
--
unstack :: [String] -> [Stream]
unstack [] = []
unstack ss = stream:unstack rest
        where
                (stream, rest) = getStream ss



-- =============================================================================
-- Internal functions
--

--------------------------------------------------------------------------------
-- Converts string to Header (if possible).
--
fromString :: String -> Maybe Header
fromString s
        | "=====" `isPrefixOf` s = Just $ drop prefixLen s
        | otherwise = Nothing
                where prefixLen = 5


--------------------------------------------------------------------------------
-- Converts list of strings into a Stream, returning whatever strings were not
-- processed.
--
getStream :: [String] -> (Stream, [String])
getStream (s:ss) = (stream, rest)
        where
                header = fromJust $ fromString s
                (sdata, rest) = break (isJust . fromString) ss 
                stream = Stream header $ map tail sdata
getStream [] = (EmptyStream, [])
