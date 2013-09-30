module Test.TestPerson (testPerson) where

import Test.HUnit

import Filters.Utils
import Person

testPerson :: Assertion
testPerson = do
       assertEqual "Parse person, no holidays" [] (vacation p1)
       assertEqual "Parse person, one holiday" [oct4] (vacation p2)
       assertEqual "Parse person, two holidays" [oct4, oct5] (vacation p3)
       where
                oct4 = stringToDay "Oct 4, 2013"
                oct5 = stringToDay "Oct 5, 2013"
                f = personFromString
                p1 = f "10\tMichael\tMobile\tMobilize\tNative\t"
                p2 = f "10\tMichael\tMobile\tMobilize\tNative\tOct 4, 2013"
                p3 = f "10\tMichael\tMobile\tMobilize\tNative\tOct 4, 2013:Oct 5, 2013"
