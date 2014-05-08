{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Control.Monad (foldM)

import Data.List (tails)
import qualified Data.Text as T
import qualified Data.Vector.Algorithms.Merge as Merge
import qualified Data.Vector.Unboxed as V
import Data.Word

import Shelly

import Statistics.Sample

import System.Console.GetOpt
import System.Environment
import System.IO

import Text.Printf

default (T.Text)

{- Commandline option parsing -}

data Command = Cmd T.Text [T.Text]
  deriving Show

data Conf = Conf {
  help          :: Bool,
  commands      :: [ Command ],
  times         :: Word32,
  chk_ret_code  :: Bool,
  cmp_average   :: Bool,
  cmp_median    :: Bool,
  cmp_geomean   :: Bool,
  cmp_diff      :: Bool,
  verbose       :: Bool
} deriving (Show)

defaultConf = Conf {
  help          = False,
  commands      = [],
  times         = 10,
  chk_ret_code  = False,
  cmp_average   = False,
  cmp_median    = False,
  cmp_geomean   = False,
  cmp_diff      = False,
  verbose       = False
}

options :: [OptDescr (Conf -> Conf)]
options =
  [ Option ['h'] ["help"]
      (NoArg (\args -> args {help=True}))
      "Show this help"
  , Option ['c'] ["command"]
      (ReqArg (\cmd_inv args -> let cmd      = Cmd cmd_name cmd_args
                                    cmd_name = T.pack $ head wds
                                    cmd_args = map T.pack $ tail wds
                                    wds      = words cmd_inv
                                in  args {commands = cmd : (commands args)})
              "<cmd>")
      "Command (including arguments) to execute."
  , Option ['v'] ["verbose"]
      (NoArg (\args -> args {verbose=True}))
      "print execution times for all command executions."
  , Option ['e'] ["chk-ret-code"]
      (NoArg (\args -> args {chk_ret_code=True}))
      "check the exit code of the programs and take only runs into the measurements that exited cleanly."
  , Option ['n'] ["times"]
      (ReqArg (\n_str args -> args {times=read n_str}) "<n>")
      ("Execute the command <n> times (default is " ++ (show $ times defaultConf) ++ ")")
  , Option ['a'] ["avg"]
      (NoArg (\args -> args {cmp_average=True}))
      "Report the arithmetic mean over all runs of each command."
  , Option ['g'] ["geomean"]
      (NoArg (\args -> args {cmp_geomean=True}))
      "Report the geometric mean over all runs of each command."
  , Option ['m'] ["median"]
      (NoArg (\args -> args {cmp_median=True}))
      "Report the median over all runs of each command."
  , Option ['d'] ["diff"]
      (NoArg (\args -> args {cmp_diff=True}))
      "Report the difference in execution times between individual commands."
  ]

helpHeader cmd_name = "Usage: " ++ cmd_name ++ " [ OPTION... ]"

parseOpts :: [String] -> IO (Maybe Conf)
parseOpts argv = case getOpt Permute options argv of
        (o, n, []   ) | null n    -> res o
                      | otherwise -> do
                            hPutStrLn stderr $ "WARNING: Unparsed arguments: " ++ (show n)
                            hPutStrLn stderr ""
                            res o
        (_, _, errs ) -> do
                      cmd_name <- getProgName
                      ioError $ userError $ concat errs ++ usageInfo (helpHeader cmd_name) options
    where
        res mods = case foldr ($) defaultConf mods of
                 conf | help conf -> do
                           cmd_name <- getProgName
                           hPutStrLn stderr $ usageInfo (helpHeader cmd_name) options
                           return Nothing
                      | otherwise -> return $ Just conf

{- Process execution -}

data ExecResult = ExecResult {
  cleanExecs :: Word32,

  avg_µs     :: Double,
  geomean_µs :: Double,
  median_µs  :: Double
}

executeCommand :: Conf -> Command -> IO ExecResult
executeCommand conf@Conf{times=n, chk_ret_code=chk, verbose=verb} (Cmd cmd args) = do
  absCmd <- if T.head cmd == '.'
            then shelly $ absPath $ fromText cmd
            else return $ fromText cmd
  let exec = errExit False $ time $ run_ absCmd args
  let cstr = show $ T.unwords $ cmd : args
  printf "\n"
  when verb $ printf "────────────────────────────────\n"
  printf "  ⇶ Executing %s\n" cstr
  when verb $ printf "┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈\n"
  times :: [Double] <- foldM (\tms i -> do
      when verb $ printf "    [ %3d of %d ] " i n
      (time, ec) <- shelly $ do
        (tm, _) <- exec
        exit_code <- lastExitCode
        return (tm, exit_code)
      if (ec == 0 || not chk)
      then do
        when verb $ printf "%.0fµs\n" $ time * 1e6
        return (time : tms)
      else do
        when verb $ printf "Non-successful execution...\n"
        return tms
    ) [] [1..n]

  times_mvec <- V.unsafeThaw $ V.fromList times
  Merge.sort times_mvec;
  times_vec <- V.unsafeFreeze times_mvec
  let numCleanExecs = fromIntegral $ V.length times_vec
      median = times_vec V.! ( fromIntegral $ numCleanExecs `div` 2) * 1e6
      avg = mean times_vec * 1e6
      geomean = geometricMean times_vec * 1e6

  when verb $ do
    printf "┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈\n"
    let ce_str :: String = printf "%d of %d" numCleanExecs n
    printf " clean execs: %12s\n" ce_str
    when (numCleanExecs > 0) $ do
      when (cmp_average conf) $ printf "     average: %10.0fµs\n" avg
      when (cmp_geomean conf) $ printf "     geomean: %10.0fµs\n" geomean
      when (cmp_median conf)  $ printf "      median: %10.0fµs\n" median

  return $ ExecResult {
    cleanExecs = numCleanExecs,
    avg_µs     = avg,
    geomean_µs = geomean,
    median_µs  = median
  }

{- Summary printing -}

printDiff :: Conf -> ((Command, ExecResult), (Command, ExecResult)) -> IO ()
printDiff conf ((Cmd c1 a1, r1), (Cmd c2 a2, r2)) = do
  let cstr1 = show $ T.unwords $ c1 : a1
  let cstr2 = show $ T.unwords $ c2 : a2
  let relDiff field = 100 * ((field r2) / (field r1) -1)

  printf                         "\n  ┌     %s\n" cstr1
  printf                           "  ├ vs. %s\n" cstr2
  when (cmp_average conf) $ printf "  │ average: %4.2f%%\n" $ relDiff avg_µs
  when (cmp_geomean conf) $ printf "  │ geomean: %4.2f%%\n" $ relDiff geomean_µs
  when (cmp_median conf)  $ printf "  │  median: %4.2f%%\n" $ relDiff median_µs

printDiffs :: Conf -> [ Command ] -> [ ExecResult ] -> IO ()
printDiffs conf cs rs = do
  let pairs l = concatMap (\(x:xs) -> zip (repeat x) xs) (filter (not . null) $ tails l)
  printf "\n────────────────────────────────\n"
  mapM_ (printDiff conf) $ pairs $ zip cs rs
  printf "\n────────────────────────────────\n"

{- CLI -}

main = do
  cmdArgs <- getArgs
  conf <- parseOpts cmdArgs
  case conf of
    Just conf -> do
      results <- mapM (executeCommand conf) $ commands conf
      when (cmp_diff conf) $ printDiffs conf (commands conf) results
    Nothing -> return ()
