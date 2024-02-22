{-
-- EPITECH PROJECT, 2024
-- Wolfram
-- File description:
-- Spec
-}

import Test.Hspec
import Display
import Parsing

main :: IO()
main = hspec $ do
    describe "Test functions" $ do

        -- Tests for getBinaryValue
        it "returns ' ' when the bit at index 0 is 0" $ getBinaryValue 0 0 `shouldBe` " "
        it "returns '*' when the bit at index 0 is 1" $ getBinaryValue 1 0 `shouldBe` "*"
        it "returns '*' when the bit at index 1 is 0" $ getBinaryValue 2 1 `shouldBe` "*"
        it "returns '*' when the bit at index 1 is 1" $ getBinaryValue 3 1 `shouldBe` "*"
        it "returns '*' when the bit at index 2 is 0" $ getBinaryValue 4 2 `shouldBe` "*"
        it "returns '*' when the bit at index 2 is 1" $ getBinaryValue 7 2 `shouldBe` "*"
        it "returns '*' when the bit at index 3 is 0" $ getBinaryValue 8 3 `shouldBe` "*"
        it "returns '*' when the bit at index 3 is 1" $ getBinaryValue 15 3 `shouldBe` "*"
        it "returns ' ' for a negative number at index 0" $ getBinaryValue (-1) 0 `shouldBe` "*"
        it "returns '*' for a negative number at index 31" $ getBinaryValue (-2147483648) 31 `shouldBe` "*"
        it "returns ' ' for a large positive number at index 30" $ getBinaryValue 1073741824 30 `shouldBe` "*"
        it "returns '*' for a large positive number at index 31" $ getBinaryValue 2147483648 31 `shouldBe` "*"

        -- Tests for getPatternRule
        it "returns ' ' when the pattern is '   '" $ getPatternRule 30 ' ' ' ' ' ' `shouldBe` " "
        it "returns '*' when the pattern is '  *'" $ getPatternRule 30 ' ' ' ' '*' `shouldBe` "*"
        it "returns '*' when the pattern is ' * '" $ getPatternRule 30 ' ' '*' ' ' `shouldBe` "*"
        it "returns '*' when the pattern is ' **'" $ getPatternRule 30 ' ' '*' '*' `shouldBe` "*"
        it "returns '*' when the pattern is '*  '" $ getPatternRule 30 '*' ' ' ' ' `shouldBe` "*"
        it "returns ' ' when the pattern is '* *'" $ getPatternRule 30 '*' ' ' '*' `shouldBe` " "
        it "returns ' ' when the pattern is '** '" $ getPatternRule 30 '*' '*' ' ' `shouldBe` " "
        it "returns ' ' when the pattern is '***'" $ getPatternRule 30 '*' '*' '*' `shouldBe` " "
        it "returns '' when the pattern is '$$$'" $ getPatternRule 30 '$' '$' '$' `shouldBe` ""
        it "returns ' ' when the pattern is '   '" $ getPatternRule 90 ' ' ' ' ' ' `shouldBe` " "
        it "returns '*' when the pattern is '  *'" $ getPatternRule 90 ' ' ' ' '*' `shouldBe` "*"
        it "returns '*' when the pattern is ' * '" $ getPatternRule 90 ' ' '*' ' ' `shouldBe` " "
        it "returns ' ' when the pattern is ' **'" $ getPatternRule 90 ' ' '*' '*' `shouldBe` "*"
        it "returns '*' when the pattern is '*  '" $ getPatternRule 90 '*' ' ' ' ' `shouldBe` "*"
        it "returns ' ' when the pattern is '* *'" $ getPatternRule 90 '*' ' ' '*' `shouldBe` " "
        it "returns '*' when the pattern is '** '" $ getPatternRule 90 '*' '*' ' ' `shouldBe` "*"
        it "returns ' ' when the pattern is '***'" $ getPatternRule 90 '*' '*' '*' `shouldBe` " "
        it "returns '' when the pattern is '$$$'" $ getPatternRule 90 '$' '$' '$' `shouldBe` ""
        it "returns ' ' when the pattern is '   '" $ getPatternRule 110 ' ' ' ' ' ' `shouldBe` " "
        it "returns '*' when the pattern is '  *'" $ getPatternRule 110 ' ' ' ' '*' `shouldBe` "*"
        it "returns ' ' when the pattern is ' * '" $ getPatternRule 110 ' ' '*' ' ' `shouldBe` "*"
        it "returns '*' when the pattern is ' **'" $ getPatternRule 110 ' ' '*' '*' `shouldBe` "*"
        it "returns '*' when the pattern is '*  '" $ getPatternRule 110 '*' ' ' ' ' `shouldBe` " "
        it "returns '*' when the pattern is '* *'" $ getPatternRule 110 '*' ' ' '*' `shouldBe` "*"
        it "returns ' ' when the pattern is '** '" $ getPatternRule 110 '*' '*' ' ' `shouldBe` "*"
        it "returns ' ' when the pattern is '***'" $ getPatternRule 110 '*' '*' '*' `shouldBe` " "
        it "returns '' when the pattern is '$$$'" $ getPatternRule 110 '$' '$' '$' `shouldBe` ""

        -- Tests for getNewStr
        it "returns '' when the string is empty" $ getNewStr "" 30 `shouldBe` ""
        it "returns ' ' when the string is ' '" $ getNewStr " " 30 `shouldBe` " "
        it "returns '  ' when the string is '  '" $ getNewStr "  " 30 `shouldBe` "  "
        it "returns '   ' when the string is '   '" $ getNewStr "   " 30 `shouldBe ` "   "
        it "returns '** ' when the string is ' * '" $ getNewStr " * " 30 `shouldBe ` "** "
        it "returns '* *' when the string is '  *'" $ getNewStr "  *" 30 `shouldBe ` "* *"
        it "returns '***' when the string is ' **'" $ getNewStr " **" 30 `shouldBe ` "***"
        it "returns '*  ' when the string is '*  '" $ getNewStr "*  " 30 `shouldBe ` "*  "
        it "returns '  *' when the string is '* *'" $ getNewStr "* *" 30 `shouldBe ` "  *"
        it "returns '** ' when the string is '** '" $ getNewStr " * " 30 `shouldBe ` "** "
        it "returns ' **' when the string is '***'" $ getNewStr "***" 30 `shouldBe ` " **"
        it "returns '   ' when the string is '   '" $ getNewStr "   " 90 `shouldBe ` "   "
        it "returns '* *' when the string is '  *'" $ getNewStr "  *" 90 `shouldBe ` "* *"
        it "returns ' * ' when the string is ' * '" $ getNewStr " * " 90 `shouldBe ` " * "
        it "returns ' **' when the string is ' **'" $ getNewStr " **" 90 `shouldBe ` "***"
        it "returns '*  ' when the string is '*  '" $ getNewStr "*  " 90 `shouldBe ` "*  "
        it "returns '  *' when the string is '* *'" $ getNewStr "* *" 90 `shouldBe ` "  *"
        it "returns '** ' when the string is '** '" $ getNewStr "** " 90 `shouldBe ` "** "
        it "returns ' **' when the string is '***'" $ getNewStr "***" 90 `shouldBe ` " **"
        it "returns '   ' when the string is '   '" $ getNewStr "   " 110 `shouldBe ` "   "
        it "returns '* *' when the string is '  *'" $ getNewStr "  *" 110 `shouldBe ` "* *"
        it "returns '** ' when the string is ' * '" $ getNewStr " * " 110 `shouldBe ` "** "
        it "returns '***' when the string is ' **'" $ getNewStr " **" 110 `shouldBe ` "***"
        it "returns '   ' when the string is '*  '" $ getNewStr "*  " 110 `shouldBe ` "   "
        it "returns '* *' when the string is '* *'" $ getNewStr "* *" 110 `shouldBe ` "* *"
        it "returns '** ' when the string is '** '" $ getNewStr "** " 110 `shouldBe ` "** "
        it "returns ' **' when the string is '***'" $ getNewStr "***" 110 `shouldBe ` " **"

        -- Tests for getFirstLine
        it "returns ' ' when the string is ' ' and the size is 0" $ getFirstLine " " 0 `shouldBe` " "
        it "returns ' ' when the string is ' ' and the size is 1" $ getFirstLine " " 1 `shouldBe` "  "
        it "returns ' * ' when the string is '*' and the size is 2" $ getFirstLine "*" 2 `shouldBe` " * "
        it "returns ' * ' when the string is '*' and the size is 3" $ getFirstLine "*" 3 `shouldBe` "  * "
        it "returns '  *' when the string is '*' and the size is 4" $ getFirstLine "*" 4 `shouldBe` "  *  "
        it "returns '                    *                    ' when the string is '*' and the size is 80" $ getFirstLine "*" 40 `shouldBe` "                    *                    "

        -- Tests for defaultConf
        it "Test defaultConf" $ defaultConf `shouldBe` Conf {
            rule = Option {optType = None, optValue = -1, hasOption = rule},
            start = Option {optType = None, optValue = 0, hasOption = start},
            line = Option {optType = None, optValue = -1, hasOption = line},
            window = Option {optType = None, optValue = 80, hasOption = window},
            move = Option {optType = None, optValue = 0, hasOption = move}}

        -- Tests for buildOpt
        it "Test buildOpt with rule -1" $ buildOpt "--rule" (Just (-1)) `shouldBe` Nothing
        it "Test buildOpt with rule 30" $ buildOpt "--rule" (Just 30) `shouldBe` Just (Option Rule 30 rule)
        it "Test buildOpt with rule 90" $ buildOpt "--rule" (Just 90) `shouldBe` Just (Option Rule 90 rule)
        it "Test buildOpt with rule 110" $ buildOpt "--rule" (Just 110) `shouldBe` Just (Option Rule 110 rule)
        it "Test buildOpt with start -1" $ buildOpt "--start" (Just (-1)) `shouldBe` Nothing
        it "Test buildOpt with start 0" $ buildOpt "--start" (Just 0) `shouldBe` Just (Option Start 0 start)
        it "Test buildOpt with start 1" $ buildOpt "--start" (Just 1) `shouldBe` Just (Option Start 1 start)
        it "Test buildOpt with line -1" $ buildOpt "--lines" (Just (-1)) `shouldBe` Nothing
        it "Test buildOpt with line 0" $ buildOpt "--lines" (Just 0) `shouldBe` Just (Option Line 0 line)
        it "Test buildOpt with line 1" $ buildOpt "--lines" (Just 1) `shouldBe` Just (Option Line 1 line)
        it "Test buildOpt with window -1" $ buildOpt "--window" (Just (-1)) `shouldBe` Nothing
        it "Test buildOpt with window 0" $ buildOpt "--window" (Just 0) `shouldBe` Just (Option Window 0 window)
        it "Test buildOpt with window 1" $ buildOpt "--window" (Just 1) `shouldBe` Just (Option Window 1 window)
        it "Test buildOpt with move -1" $ buildOpt "--move" (Just (-1)) `shouldBe` Just (Option Move (-1) move)
        it "Test buildOpt with move 0" $ buildOpt "--move" (Just 0) `shouldBe` Just (Option Move 0 move)
        it "Test buildOpt with move 1" $ buildOpt "--move" (Just 1) `shouldBe` Just (Option Move 1 move)
        it "Test buildOpt with random string" $ buildOpt "random" (Just 1) `shouldBe` Nothing

        -- Tests for fillConf
        it "Test fillConf with rule" $ fillConf defaultConf (Just (Option Rule 30 rule)) `shouldBe` Just (Conf {
            rule = Option Rule 30 rule,
            start = Option {optType = None, optValue = 0, hasOption = start},
            line = Option {optType = None, optValue = -1, hasOption = line},
            window = Option {optType = None, optValue = 80, hasOption = window},
            move = Option {optType = None, optValue = 0, hasOption = move}})
        it "Test fillConf with start" $ fillConf defaultConf (Just (Option Start 1 start)) `shouldBe` Just (Conf {
            rule = Option {optType = None, optValue = -1, hasOption = rule},
            start = Option Start 1 start,
            line = Option {optType = None, optValue = -1, hasOption = line},
            window = Option {optType = None, optValue = 80, hasOption = window},
            move = Option {optType = None, optValue = 0, hasOption = move}})
        it "Test fillConf with line" $ fillConf defaultConf (Just (Option Line 1 line)) `shouldBe` Just (Conf {
            rule = Option {optType = None, optValue = -1, hasOption = rule},
            start = Option {optType = None, optValue = 0, hasOption = start},
            line = Option Line 1 line,
            window = Option {optType = None, optValue = 80, hasOption = window},
            move = Option {optType = None, optValue = 0, hasOption = move}})
        it "Test fillConf with window" $ fillConf defaultConf (Just (Option Window 1 window)) `shouldBe` Just (Conf {
            rule = Option {optType = None, optValue = -1, hasOption = rule},
            start = Option {optType = None, optValue = 0, hasOption = start},
            line = Option {optType = None, optValue = -1, hasOption = line},
            window = Option Window 1 window,
            move = Option {optType = None, optValue = 0, hasOption = move}})
        it "Test fillConf with move" $ fillConf defaultConf (Just (Option Move 1 move)) `shouldBe` Just (Conf {
            rule = Option {optType = None, optValue = -1, hasOption = rule},
            start = Option {optType = None, optValue = 0, hasOption = start},
            line = Option {optType = None, optValue = -1, hasOption = line},
            window = Option {optType = None, optValue = 80, hasOption = window},
            move = Option Move 1 move})
        it "Test fillConf with random string" $ fillConf defaultConf (Just (Option None 1 rule)) `shouldBe` Nothing

        -- Tests for getOpts
        it "Test getOpts with no conf" $ getOpts Nothing [] `shouldBe` Nothing
        it "Test getOpts with no conf and one option" $ getOpts Nothing ["--rule", "30"] `shouldBe` Nothing
        it "Test getOpts with a conf and no option" $ getOpts (Just defaultConf) [] `shouldBe` Just defaultConf
        it "Test getOpts with a conf and one option" $ getOpts (Just defaultConf) ["--rule", "30"] `shouldBe` Just (Conf {
            rule = Option Rule 30 rule,
            start = Option {optType = None, optValue = 0, hasOption = start},
            line = Option {optType = None, optValue = -1, hasOption = line},
            window = Option {optType = None, optValue = 80, hasOption = window},
            move = Option {optType = None, optValue = 0, hasOption = move}})
        it "Test getOpts with a conf and multiple options" $ getOpts (Just defaultConf) ["--rule", "30", "--start", "1", "--lines", "1", "--window", "1", "--move", "1"] `shouldBe` Just (Conf {
            rule = Option Rule 30 rule,
            start = Option Start 1 start,
            line = Option Line 1 line,
            window = Option Window 1 window,
            move = Option Move 1 move})
        it "Test getOpts with a conf and multiple options and a random string" $ getOpts (Just defaultConf) ["--rule", "30", "--start", "1", "--lines", "1", "--window", "1", "--move", "1", "random", "random"] `shouldBe` Nothing

        -- Tests for checkRuleSet
        it "Test checkRuleSet with rule 30" $ checkRuleSet (Conf {
            rule = Option Rule 30 rule,
            start = Option {optType = None, optValue = 0, hasOption = start},
            line = Option {optType = None, optValue = -1, hasOption = line},
            window = Option {optType = None, optValue = 80, hasOption = window},
            move = Option {optType = None, optValue = 0, hasOption = move}}) `shouldBe` Just (Conf {
            rule = Option Rule 30 rule,
            start = Option {optType = None, optValue = 0, hasOption = start},
            line = Option {optType = None, optValue = -1, hasOption = line},
            window = Option {optType = None, optValue = 80, hasOption = window},
            move = Option {optType = None, optValue = 0, hasOption = move}})
        it "Test checkRuleSet with rule 90" $ checkRuleSet (Conf {
            rule = Option Rule 90 rule,
            start = Option {optType = None, optValue = 0, hasOption = start},
            line = Option {optType = None, optValue = -1, hasOption = line},
            window = Option {optType = None, optValue = 80, hasOption = window},
            move = Option {optType = None, optValue = 0, hasOption = move}}) `shouldBe` Just (Conf {
            rule = Option Rule 90 rule,
            start = Option {optType = None, optValue = 0, hasOption = start},
            line = Option {optType = None, optValue = -1, hasOption = line},
            window = Option {optType = None, optValue = 80, hasOption = window},
            move = Option {optType = None, optValue = 0, hasOption = move}})
        it "Test checkRuleSet with rule 110" $ checkRuleSet (Conf {
            rule = Option Rule 110 rule,
            start = Option {optType = None, optValue = 0, hasOption = start},
            line = Option {optType = None, optValue = -1, hasOption = line},
            window = Option {optType = None, optValue = 80, hasOption = window},
            move = Option {optType = None, optValue = 0, hasOption = move}}) `shouldBe` Just (Conf {
            rule = Option Rule 110 rule,
            start = Option {optType = None, optValue = 0, hasOption = start},
            line = Option {optType = None, optValue = -1, hasOption = line},
            window = Option {optType = None, optValue = 80, hasOption = window},
            move = Option {optType = None, optValue = 0, hasOption = move}})
        it "Test checkRuleSet with rule 1" $ checkRuleSet (Conf {
            rule = Option Rule 1 rule,
            start = Option {optType = None, optValue = 0, hasOption = start},
            line = Option {optType = None, optValue = -1, hasOption = line},
            window = Option {optType = None, optValue = 80, hasOption = window},
            move = Option {optType = None, optValue = 0, hasOption = move}}) `shouldBe` Nothing

        -- Tests for getRuleValue
        it "Test getRuleValue with rule 30" $ getRuleValue (Conf {
            rule = Option Rule 30 rule,
            start = Option {optType = None, optValue = 0, hasOption = start},
            line = Option {optType = None, optValue = -1, hasOption = line},
            window = Option {optType = None, optValue = 80, hasOption = window},
            move = Option {optType = None, optValue = 0, hasOption = move}}) `shouldBe` 30

        -- Tests for getStartValue
        it "Test getStartValue with start 0" $ getStartValue (Conf {
            rule = Option Rule 30 rule,
            start = Option Start 5 start,
            line = Option {optType = None, optValue = -1, hasOption = line},
            window = Option {optType = None, optValue = 80, hasOption = window},
            move = Option {optType = None, optValue = 0, hasOption = move}}) `shouldBe` 5

        -- Tests for getLinesValue
        it "Test getLineValue with line 0" $ getLinesValue (Conf {
            rule = Option Rule 30 rule,
            start = Option {optType = None, optValue = 0, hasOption = start},
            line = Option Line 5 line,
            window = Option {optType = None, optValue = 80, hasOption = window},
            move = Option {optType = None, optValue = 0, hasOption = move}}) `shouldBe` 5

        -- Tests for getWindowSize
        it "Test getWindowSize with window 0" $ getWindowSize (Conf {
            rule = Option Rule 30 rule,
            start = Option {optType = None, optValue = 0, hasOption = start},
            line = Option {optType = None, optValue = -1, hasOption = line},
            window = Option Window 5 window,
            move = Option {optType = None, optValue = 0, hasOption = move}}) `shouldBe` 5

        -- Tests for getMoveValue
        it "Test getMoveValue with move 0" $ getMoveValue (Conf {
            rule = Option Rule 30 rule,
            start = Option {optType = None, optValue = 0, hasOption = start},
            line = Option {optType = None, optValue = -1, hasOption = line},
            window = Option {optType = None, optValue = 80, hasOption = window},
            move = Option Move 5 move}) `shouldBe` 5