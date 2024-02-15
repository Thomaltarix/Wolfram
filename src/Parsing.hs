--
-- EPITECH PROJECT, 2024
-- Wolfram
-- File description:
-- Parsing
--

module Parsing (handleErrors) where

import System.Exit (exitWith, ExitCode(ExitFailure))

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
defaultConf = Conf {rule = Option {optType = None, optValue = -1, hasOption = rule},
                    start = Option {optType = None, optValue = 0, hasOption = start},
                    line = Option {optType = None, optValue = -1, hasOption = line},
                    window = Option {optType = None, optValue = 80, hasOption = window},
                    move = Option {optType = None, optValue = 0, hasOption = move}}

buildOpt :: String -> Int -> Maybe Option
buildOpt "--rule" value = Just (Option Rule value rule)
buildOpt "--start" value = Just (Option Start value start)
buildOpt "--lines" value = Just (Option Line value line)
buildOpt "--window" value = Just (Option Window value window)
buildOpt "--move" value = Just (Option Move value move)
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
    let newOpt = buildOpt opt (read value :: Int)
    newConf <- fillConf conf newOpt
    getOpts (Just newConf) opts

isRuleSet :: Int -> Bool
isRuleSet 30 = True
isRuleSet 90 = True
isRuleSet 110 = True
isRuleSet _ = False

checkRuleSet :: Conf -> IO ()
checkRuleSet (Conf {rule = Option {optValue = ruleSet}}) =
    if isRuleSet ruleSet then return ()
    else exitWith (ExitFailure 84)

handleErrors :: [String] -> IO ()
handleErrors [] = exitWith (ExitFailure 84)
handleErrors args = let conf = getOpts (Just defaultConf) args in
                        case conf of
                            Nothing -> exitWith (ExitFailure 84)
                            Just conf2 -> checkRuleSet conf2
