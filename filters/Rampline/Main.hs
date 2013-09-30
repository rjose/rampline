-----------------------------------------------------------------------------
-- |
-- Module      :  Rampline 
-- Copyright   :  (c) Rino Jose 2013
-- License     :  BSD-style
-- 
-- Maintainer  :  @rjose
-- Stability   :  experimental
-- Portability :  portable
--
-- Provides ability to consume structured data from stdin and generate text
-- reports, data for vending machine apps, or data for broadcast for quarterly
-- planning applications.
--
-----------------------------------------------------------------------------

-- =============================================================================
-- Module definition
--
module Main (main) where
 
-- =============================================================================
-- Module imports
--
import Data.List
import Data.Maybe
import System.Console.GetOpt
import System.Environment
import System.Exit

import SkillAmount
import StackStream
import Work
import Text.Printf
import Text.JSON

import qualified Filters.RamplineApp as RamplineApp

-- =============================================================================
-- Data types
--

--------------------------------------------------------------------------------
-- Changes behavior of filters when program is run.
--
--      NOTE: By default, the app will expect Work and Staff streams and will
--      generate JSON data for the qplan web app.
--
data Flag
        = Chart String
        | Raw
        | Data
        | Sample
          deriving (Eq, Show)


-- =============================================================================
-- Public API
--

--------------------------------------------------------------------------------
-- | Gets commandline args and passes them to "run".
--
main = getArgs >>= run


-- =============================================================================
-- Internal functions
--

--------------------------------------------------------------------------------
-- Reads content from stdin, transforms it, and writes it to stdout.
--
run :: [String] -> IO ()
run args = do
                contents <- getContents
                putStr $ computeResult flags contents
        where
                (flags, _, _) = getOpt Permute options args
                options =
                  [ Option ['c'] ["chart"] (ReqArg chart "CHART")
                                                     "construct chart data",
                    Option ['r'] ["raw"] (NoArg Raw) "generate raw output"
                  ]
                chart = Chart


--------------------------------------------------------------------------------
-- Processes flags and creates output.
--
computeResult :: [Flag] -> String -> String
computeResult flags contents
        | Raw `elem` flags = contents
        | Sample `elem` flags = RamplineApp.filterString contents
        | otherwise = RamplineApp.filterString contents
