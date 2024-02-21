{-
-- EPITECH PROJECT, 2024
-- Wolfram
-- File description:
-- Parsing
-}

module Parsing (getRule,
                getStart,
                getLines,
                getWindow,
                getMove,
                getCharacter,
                getOpts,
                defaultConf,
                checkRuleSet) where

import Text.Read (readMaybe)

data Type = Rule | Start | Line | Window | Move | Character | None

data Option = Option {  optType :: Type,
                        optValue :: Int,
                        hasOption :: Conf -> Option}

data Conf = Conf {  rule :: Option,
                    start :: Option,
                    line :: Option,
                    window :: Option,
                    move :: Option,
                    character :: Option}

defaultConf :: Conf
defaultConf = Conf {
    rule = Option {optType = None, optValue = -1, hasOption = rule},
    start = Option {optType = None, optValue = 0, hasOption = start},
    line = Option {optType = None, optValue = -1, hasOption = line},
    window = Option {optType = None, optValue = 80, hasOption = window},
    move = Option {optType = None, optValue = 0, hasOption = move},
    character = Option {optType = None, optValue = 42, hasOption = character}}

buildOpt :: String -> Maybe Int -> Maybe Option
buildOpt _ Nothing = Nothing
buildOpt "--rule" (Just value)
    | value >= 0 && value <= 255 = Just (Option Rule value rule)
    | otherwise = Nothing
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
buildOpt "--char" (Just value)
    |  value < 32 || value > 126 = Nothing
    | otherwise = Just (Option Character value character)
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
fillConf conf (Just opt@(Option Character _ hasOpt)) = case hasOpt conf of
                            (Option None _ _) -> Just (conf {character = opt})
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
checkRuleSet conf@(Conf {rule = Option {optValue = value}})
    | value >= 0 && value <= 255 = Just conf
    | otherwise = Nothing

getRule :: Conf -> Int
getRule (Conf {rule = Option {optValue = ruleSet}}) = ruleSet

getStart :: Conf -> Int
getStart (Conf {start = Option {optValue = startSet}}) = startSet

getLines :: Conf -> Int
getLines (Conf {line = Option {optValue = linesSet}}) = linesSet

getWindow :: Conf -> Int
getWindow (Conf {window = Option {optValue = windowSet}}) = windowSet

getMove :: Conf -> Int
getMove (Conf {move = Option {optValue = moveSet}}) = moveSet

getCharacter :: Conf -> Int
getCharacter (Conf {character = Option {optValue = charSet}}) = charSet
