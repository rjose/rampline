module Test.TestSkillAmount (testSkillAmount) where

import Test.HUnit
import SkillAmount

testSkillAmount :: Assertion
testSkillAmount = do
       assertEqual "Should parse simple estimate string"
                (fromString "Apps:S")
                (SkillAmount "Apps" "S" 1)

       assertEqual "Should parse estimate string with factor"
                (fromString "Apps:3L")
                (SkillAmount "Apps" "3L" 9)

       assertEqual "Should parse vector string"
                (fromVectorString "Apps:S,Native:M,QA:3S")
                [
                        (SkillAmount "Apps" "S" 1),
                        (SkillAmount "Native" "M" 2),
                        (SkillAmount "QA" "3S" 3)
                ]

       assertEqual "Should sum skill amounts properly"
                (skillSum skills)
                [
                        (SkillSum "Apps" 3),
                        (SkillSum "QA" 3)
                ]

       assertEqual "Should take difference of skill amounts properly"
                (skillDifference skills skills2)
                [
                        (SkillSum "Apps" 2),
                        (SkillSum "Native" (-13)),
                        (SkillSum "QA" 3)
                ]
       where
                skills = [
                           (SkillAmount "Apps" "S" 1),
                           (SkillAmount "Apps" "M" 2),
                           (SkillAmount "QA" "3S" 3)
                         ]
                skills2 = [
                           (SkillAmount "Apps" "S" 1),
                           (SkillAmount "Native" "Q" 13)
                         ]
