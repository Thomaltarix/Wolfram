{-
-- EPITECH PROJECT, 2024
-- Wolfram
-- File description:
-- Parsing
-}

module Parsing (getRuleValue,
                getStartValue,
                getLinesValue,
                getWindowSize,
                getMoveValue,
                getOpts,
                defaultConf,
                checkRuleSet) where

import Text.Read (readMaybe)

data Type = Rule | Start | Line | Window | Move | None

data Option = Option {  optType :: Type,
                        optValue :: Int,
                        hasOption :: Conf -> Option}

data Conf = Conf {  rule :: Option,
                    start :: Option,
                    line :: Option,
                    window :: Option,
                    move :: Option}

defaultConf :: Conf
defaultConf = Conf {
    rule = Option {optType = None, optValue = -1, hasOption = rule},
    start = Option {optType = None, optValue = 0, hasOption = start},
    line = Option {optType = None, optValue = -1, hasOption = line},
    window = Option {optType = None, optValue = 80, hasOption = window},
    move = Option {optType = None, optValue = 0, hasOption = move}}

buildOpt :: String -> Maybe Int -> Maybe Option
buildOpt _ Nothing = Nothing
buildOpt "--rule" (Just 30) = Just (Option Rule 30 rule)
buildOpt "--rule" (Just 90) = Just (Option Rule 90 rule)
buildOpt "--rule" (Just 110) = Just (Option Rule 110 rule)
buildOpt "--start" (Just value)
    | value < 0 = Nothing
    | otherwise = Just (Option Start value start)
buildOpt "--lines" (Just value)
    | value < 0 = Nothing
    | otherwise = Just (Option Line value line)
buildOpt "--window" (Just value)
    | value < 0 = Nothing
    | otherwise = Just (Option Window value window)
buildOpt "--move" (Just value) = Just (Option Move value move)
buildOpt _ _ = Nothing

fillConf :: Conf -> Maybe Option -> Maybe Conf
fillConf _ Nothing = Nothing
fillConf conf (Just opt@(Option Rule _ hasOpt)) = case hasOpt conf of
                            (Option None _ _) -> Just (conf {rule = opt})
                            _ -> Nothing
fillConf conf (Just opt@(Option Start _ hasOpt)) = case hasOpt conf of
                            (Option None _ _) -> Just (conf {start = opt})
                            _ -> Nothing
fillConf conf (Just opt@(Option Line _ hasOpt)) = case hasOpt conf of
                            (Option None _ _) -> Just (conf {line = opt})
                            _ -> Nothing
fillConf conf (Just opt@(Option Window _ hasOpt)) = case hasOpt conf of
                            (Option None _ _) -> Just (conf {window = opt})
                            _ -> Nothing
fillConf conf (Just opt@(Option Move _ hasOpt)) = case hasOpt conf of
                            (Option None _ _) -> Just (conf {move = opt})
                            _ -> Nothing
fillConf _ _ = Nothing

getOpts ::  Maybe Conf -> [String] -> Maybe Conf
getOpts Nothing _ = Nothing
getOpts (Just conf) [] = Just conf
getOpts (Just _) [_] = Nothing
getOpts (Just conf) (opt:value:opts) = do
    newConf <- fillConf conf (buildOpt opt (readMaybe value :: Maybe Int))
    getOpts (Just newConf) opts

checkRuleSet :: Conf -> Maybe Conf
checkRuleSet conf@(Conf {rule = Option {optValue = 30}}) = Just conf
checkRuleSet conf@(Conf {rule = Option {optValue = 90}}) = Just conf
checkRuleSet conf@(Conf {rule = Option {optValue = 110}}) = Just conf
checkRuleSet _ = Nothing

getRuleValue :: Conf -> Int
getRuleValue (Conf {rule = Option {optValue = ruleSet}}) = ruleSet

getStartValue :: Conf -> Int
getStartValue (Conf {start = Option {optValue = startSet}}) = startSet

getLinesValue :: Conf -> Int
getLinesValue (Conf {line = Option {optValue = linesSet}}) = linesSet

getWindowSize :: Conf -> Int
getWindowSize (Conf {window = Option {optValue = windowSet}}) = windowSet

getMoveValue :: Conf -> Int
getMoveValue (Conf {move = Option {optValue = moveSet}}) = moveSet
