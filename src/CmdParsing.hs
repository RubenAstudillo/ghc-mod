{-# LANGUAGE DeriveDataTypeable #-}

module CmdParsing where

import Language.Haskell.GhcMod
import Control.Exception
import Data.Typeable
import System.Console.GetOpt

parseArgs :: [OptDescr (Options -> Options)] -> [String] -> (Options, [String])
parseArgs spec argv
    = case getOpt Permute spec argv of
        (o,n,[]  ) -> (foldr id defaultOptions o, n)
        (_,_,errs) -> throw (CmdArg errs)

----------------------------------------------------------------

data CommandError = SafeList
                  | ArgumentsMismatch String
                  | NoSuchCommand String
                  | CmdArg [String]
                  | FileNotExist String deriving (Show, Typeable)

instance Exception CommandError

----------------------------------------------------------------

optToLisp :: OptDescr (Options -> Options)
optToLisp = Option "l" ["tolisp"]
    (NoArg (\opts -> opts { outputStyle = LispStyle }))
    "print as a list of Lisp"

optHlintOpt :: OptDescr (Options -> Options)
optHlintOpt = Option "h" ["hlintOpt"]
    (ReqArg (\h opts -> opts { hlintOpts = h : hlintOpts opts }) "hlintOpt")
    "hlint options"

optGhcOpt :: OptDescr (Options -> Options)
optGhcOpt = Option "g" ["ghcOpt"]
    (ReqArg (\g opts -> opts { ghcUserOptions = g : ghcUserOptions opts }) "ghcOpt")
    "GHC options"

optVerbose :: OptDescr (Options -> Options)
optVerbose = Option "v" ["verbose"]
    (NoArg (\opts -> opts { ghcUserOptions = "-v" : ghcUserOptions opts }))
    "verbose"

optOperators :: OptDescr (Options -> Options)
optOperators = Option "o" ["operators"]
    (NoArg (\opts -> opts { operators = True }))
    "print operators, too"

optDetailed :: OptDescr (Options -> Options)
optDetailed = Option "d" ["detailed"]
    (NoArg (\opts -> opts { detailed = True }))
    "print detailed info"

optQualified :: OptDescr (Options -> Options)
optQualified = Option "q" ["qualified"]
    (NoArg (\opts -> opts { qualified = True }))
    "show qualified names"

optBoundary :: OptDescr (Options -> Options)
optBoundary = Option "b" ["boundary"]
    (ReqArg (\s opts -> opts { lineSeparator = LineSeparator s }) "sep")
    "specify line separator (default is Nul string)"
