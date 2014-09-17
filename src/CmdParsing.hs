{-# LANGUAGE DeriveDataTypeable #-}

module CmdParsing (
    parseArgs
  , CommandError (..)
  , optToLisp
  , optHlintOpt
  , optGhcOpt
  , optVerbose
  , optOperators
  , optDetailed
  , optQualified
  , optBoundary
  ) where

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

option :: String -> [String] -> String -> ArgDescr a -> OptDescr a
option s l udsc dsc = Option s l dsc udsc

reqArg :: String -> (String -> a) -> ArgDescr a
reqArg udsc dsc = ReqArg dsc udsc

----------------------------------------------------------------

optToLisp :: OptDescr (Options -> Options)
optToLisp = option "l" ["tolisp"] "print as a list of Lisp" $
               NoArg $ \o -> o { outputStyle = LispStyle }

optHlintOpt :: OptDescr (Options -> Options)
optHlintOpt = option "h" ["hlintOpt"] "hlint options" $
                reqArg "hlintOpt" $ \h o -> o { hlintOpts = h : hlintOpts o }

optGhcOpt :: OptDescr (Options -> Options)
optGhcOpt = option "g" ["ghcOpt"] "GHC options" $
               reqArg "ghcOpt" $ \g o ->
                   o { ghcUserOptions = g : ghcUserOptions o }

optVerbose :: OptDescr (Options -> Options)
optVerbose = option "v" ["verbose"] "verbose" $
               NoArg $ \o -> o { ghcUserOptions = "-v" : ghcUserOptions o }

optOperators :: OptDescr (Options -> Options)
optOperators = option "o" ["operators"] "print operators, too" $
                 NoArg $ \o -> o { operators = True }

optDetailed :: OptDescr (Options -> Options)
optDetailed = option "d" ["detailed"] "print detailed info" $
               NoArg $ \o -> o { detailed = True }

optQualified :: OptDescr (Options -> Options)
optQualified = option "q" ["qualified"] "show qualified names" $
                 NoArg $ \o -> o { qualified = True }

optBoundary :: OptDescr (Options -> Options)
optBoundary = option "b" ["boundary"] "specify line separator (default is Nul string)" $
                reqArg "sep" $ \s o -> o { lineSeparator = LineSeparator s }
