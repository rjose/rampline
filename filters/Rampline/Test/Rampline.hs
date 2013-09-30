import Data.Monoid
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import StackStream

import Test.TestPerson
import Test.TestSchedule
import Test.TestSkillAmount
import Test.TestWork

main :: IO ()
main = defaultMainWithOpts
        [ testCase "rev" testRev,
          testCase "unstack" test_unstack,
          testCase "TestSkillAmount" testSkillAmount,
          testCase "TestWork" testWork,
          testCase "TestSchedule" testSchedule,
          testCase "TestPerson" testPerson
        ] mempty

testRev :: Assertion
testRev = reverse [1, 2, 3] @?= [3, 2, 1]

test_unstack = do
        assertEqual "Should have 3 streams" 3 (length streams)
        assertEqual "Should have right headers"
                ["title", "demand", "shortage"] headers
        where
                streams = unstack linput
                headers = map streamHeader streams


-- Support

-- Returns header string for a stream
streamHeader :: Stream -> String
streamHeader (Stream h _) = h
streamHeader _ = ""


-- DATA

input="=====title\n" ++
      "\tShortage Chart (with shortage)\n" ++
      "=====demand\n" ++
      "\tTrack 1\t20\n" ++
      "\tTrack 2\t35\n" ++
      "=====shortage\n" ++
      "\tApps\t5\n" ++
      "\tNative\t3\n" ++
      "\tQA\t1"

linput = lines input
