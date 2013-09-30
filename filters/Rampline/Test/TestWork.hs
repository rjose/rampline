module Test.TestWork (testWork) where

import Test.HUnit
import Work

testWork :: Assertion
testWork = do
       assertEqual "Should parse work item" "Track1" (track work)
       assertEqual "Should parse empty line" P3 (triage empty_work)

       where
                workline = "ABC123\t30\tAn item of work\tApps:S,Native:M,QA:3S\t1.5\tTrack1\tMobile\t8\tB21,C23"
                emptyWorkline = "\t\t\t\t\t\t\t\t"
                work = workFromString workline
                empty_work = workFromString emptyWorkline
