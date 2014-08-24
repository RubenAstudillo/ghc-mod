{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Config (cProjectVersion)
import Control.Applicative ((<$>))
import Control.Exception (Handler(..), ErrorCall(..))
import CoreMonad (liftIO)
import qualified Control.Exception as E
import Data.Version (showVersion)
import Language.Haskell.GhcMod
import Paths_ghc_mod
import CmdParsing
import System.Console.GetOpt (OptDescr(..))
import qualified System.Console.GetOpt as O
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStr, hPutStrLn, stdout, stderr, hSetEncoding, utf8)

----------------------------------------------------------------

progVersion :: String
progVersion = "ghc-mod version " ++ showVersion version ++ " compiled by GHC " ++ cProjectVersion ++ "\n"

ghcOptHelp :: String
ghcOptHelp = " [-g GHC_opt1 -g GHC_opt2 ...] "

usage :: String
usage =    progVersion
        ++ "Usage:\n"
        ++ "\t ghc-mod list" ++ ghcOptHelp ++ "[-l] [-d]\n"
        ++ "\t ghc-mod lang [-l]\n"
        ++ "\t ghc-mod flag [-l]\n"
        ++ "\t ghc-mod browse" ++ ghcOptHelp ++ "[-l] [-o] [-d] [-q] [<package>:]<module> [[<package>:]<module> ...]\n"
        ++ "\t ghc-mod check" ++ ghcOptHelp ++ "<HaskellFiles...>\n"
        ++ "\t ghc-mod expand" ++ ghcOptHelp ++ "<HaskellFiles...>\n"
        ++ "\t ghc-mod debug" ++ ghcOptHelp ++ "\n"
        ++ "\t ghc-mod info" ++ ghcOptHelp ++ "<HaskellFile> <module> <expression>\n"
        ++ "\t ghc-mod type" ++ ghcOptHelp ++ "<HaskellFile> <module> <line-no> <column-no>\n"
        ++ "\t ghc-mod split" ++ ghcOptHelp ++ "<HaskellFile> <module> <line-no> <column-no>\n"
        ++ "\t ghc-mod sig" ++ ghcOptHelp ++ "<HaskellFile> <module> <line-no> <column-no>\n"
        ++ "\t ghc-mod refine" ++ ghcOptHelp ++ "<HaskellFile> <module> <line-no> <column-no> <expression>\n"
        ++ "\t ghc-mod auto" ++ ghcOptHelp ++ "<HaskellFile> <module> <line-no> <column-no>\n"
        ++ "\t ghc-mod find <symbol>\n"
        ++ "\t ghc-mod lint [-h opt] <HaskellFile>\n"
        ++ "\t ghc-mod root\n"
        ++ "\t ghc-mod doc <module>\n"
        ++ "\t ghc-mod boot\n"
        ++ "\t ghc-mod version\n"
        ++ "\t ghc-mod help\n"
        ++ "\n"
        ++ "<module> for \"info\" and \"type\" is not used, anything is OK.\n"
        ++ "It is necessary to maintain backward compatibility.\n"

----------------------------------------------------------------

argspec :: [OptDescr (Options -> Options)]
argspec = 
    [ optToLisp
    , optHlintOpt
    , optGhcOpt
    , optVerbose
    , optOperators
    , optDetailed
    , optQualified
    , optBoundary ]

----------------------------------------------------------------

main :: IO ()
main = flip E.catches handlers $ do
-- #if __GLASGOW_HASKELL__ >= 611
    hSetEncoding stdout utf8
-- #endif
    args <- getArgs
    let (opt,cmdArg) = parseArgs argspec args
    let cmdArg0 = cmdArg !. 0
        cmdArg1 = cmdArg !. 1
        cmdArg3 = cmdArg !. 3
        cmdArg4 = cmdArg !. 4
        cmdArg5 = cmdArg !. 5
        remainingArgs = tail cmdArg
        nArgs n f = if length remainingArgs == n
                        then f
                        else E.throw (ArgumentsMismatch cmdArg0)
    (res, _) <- runGhcModT opt $ case cmdArg0 of
      "list"    -> modules
      "lang"    -> languages
      "flag"    -> flags
      "browse"  -> concat <$> mapM browse remainingArgs
      "check"   -> checkSyntax remainingArgs
      "expand"  -> expandTemplate remainingArgs
      "debug"   -> debugInfo
      "info"    -> nArgs 3 info cmdArg1 cmdArg3
      "type"    -> nArgs 4 $ types cmdArg1 (read cmdArg3) (read cmdArg4)
      "split"   -> nArgs 4 $ splits cmdArg1 (read cmdArg3) (read cmdArg4)
      "sig"     -> nArgs 4 $ sig cmdArg1 (read cmdArg3) (read cmdArg4)
      "refine"  -> nArgs 5 $ refine cmdArg1 (read cmdArg3) (read cmdArg4) cmdArg5
      "auto"    -> nArgs 4 $ auto cmdArg1 (read cmdArg3) (read cmdArg4)
      "find"    -> nArgs 1 $ findSymbol cmdArg1
      "lint"    -> nArgs 1 $ withFile lint cmdArg1
      "root"    -> rootInfo
      "doc"     -> nArgs 1 $ pkgDoc cmdArg1
      "dumpsym" -> dumpSymbol
      "boot"    -> boot
      "version" -> return progVersion
      "help"    -> return $ O.usageInfo usage argspec
      cmd       -> E.throw (NoSuchCommand cmd)
    case res of
      Right s -> putStr s
      Left (GMENoMsg) ->
          hPutStrLn stderr "Unknown error"
      Left (GMEString msg) ->
          hPutStrLn stderr msg
      Left (GMECabalConfigure msg) ->
          hPutStrLn stderr $ "cabal configure failed: " ++ msg
  where
    handlers = [Handler (handleThenExit handler1), Handler (handleThenExit handler2)]
    handleThenExit handler e = handler e >> exitFailure
    handler1 :: ErrorCall -> IO ()
    handler1 = print -- for debug
    handler2 :: CommandError -> IO ()
    handler2 SafeList = printUsage
    handler2 (ArgumentsMismatch cmd) = do
        hPutStrLn stderr $ "\"" ++ cmd ++ "\": Arguments did not match"
        printUsage
    handler2 (NoSuchCommand cmd) = do
        hPutStrLn stderr $ "\"" ++ cmd ++ "\" not supported"
        printUsage
    handler2 (CmdArg errs) = do
        mapM_ (hPutStr stderr) errs
        printUsage
    handler2 (FileNotExist file) = do
        hPutStrLn stderr $ "\"" ++ file ++ "\" not found"
        printUsage
    printUsage = hPutStrLn stderr $ '\n' : O.usageInfo usage argspec
    withFile :: IOish m => (FilePath -> GhcModT m a) -> FilePath -> GhcModT m a
    withFile cmd file = do
        exist <- liftIO $ doesFileExist file
        if exist
            then cmd file
            else E.throw (FileNotExist file)
    xs !. idx
      | length xs <= idx = E.throw SafeList
      | otherwise = xs !! idx
