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
  cmp_mean      :: Bool,
  cmp_median    :: Bool,
  cmp_geomean   :: Bool,
  cmp_diff      :: Bool,
  verbose       :: Bool,
  interleaved   :: Bool
} deriving (Show)

defaultConf = Conf {
  help          = False,
  commands      = [],
  times         = 10,
  chk_ret_code  = False,
  cmp_mean      = False,
  cmp_median    = False,
  cmp_geomean   = False,
  cmp_diff      = False,
  verbose       = False,
  interleaved   = False
}

options :: [OptDescr ((Conf, Maybe Command) -> (Conf, Maybe Command))]
options =
  [ Option ['h'] ["help"]
      (NoArg (\(args, t) -> (args {help=True}, t)))
      "Show this help"
  , Option ['c'] ["command"]
      (ReqArg (\cmd_inv (args, t) -> let cmd      = Cmd cmd_name cmd_args
                                         cmd_name = T.pack $ head wds
                                         cmd_args = map T.pack $ tail wds
                                         wds      = words cmd_inv
                                     in  (args {commands = cmd : (commands args)}, t))
              "<cmd>")
      "Command (including arguments) to execute."
  , Option ['t'] ["cmd-template"]
      (ReqArg (\cmd_inv (args, t) -> let cmd      = Cmd cmd_name cmd_args
                                         cmd_name = T.pack $ head wds
                                         cmd_args = map T.pack $ tail wds
                                         wds      = words cmd_inv
                                     in  (args, Just cmd))
              "<cmd>")
      "Partial command (possibly including arguments) to execute.\nThe following options (given via '-o') will be combined with this template to form a command."
  , Option ['o'] ["options"]
      (ReqArg (\opts (args, t) -> let nargs = case t of
                                         Nothing                -> args
                                         (Just (Cmd cmd cargs)) -> args {commands = nCmd : (commands args)} where
                                             nCmd = Cmd cmd (cargs ++ addArgs)
                                             addArgs = map T.pack $ words opts
                                  in  (nargs, t))
              "<args>")
      "Command arguments to combine with the last command template (given via '-t')."
  , Option ['v'] ["verbose"]
      (NoArg (\(args, t) -> (args {verbose=True}, t)))
      "Print execution times for all command executions."
  , Option ['e'] ["chk-ret-code"]
      (NoArg (\(args, t) -> (args {chk_ret_code=True}, t)))
      "Check the exit code of the programs and take only runs into the measurements that exited cleanly."
  , Option ['i'] ["interleaved"]
      (NoArg (\(args, t) -> (args {interleaved=True}, t)))
      "Execute the instances of individual commands interleaved (instead of one command after the other)."
  , Option ['n'] ["times"]
      (ReqArg (\n_str (args, t) -> (args {times=read n_str}, t)) "<n>")
      ("Execute the command <n> times (default is " ++ (show $ times defaultConf) ++ ")")
  , Option ['a'] ["avg"]
      (NoArg (\(args, t) -> (args {cmp_mean=True}, t)))
      "Report the arithmetic mean over all runs of each command."
  , Option ['g'] ["geomean"]
      (NoArg (\(args, t) -> (args {cmp_geomean=True}, t)))
      "Report the geometric mean over all runs of each command."
  , Option ['m'] ["median"]
      (NoArg (\(args, t) -> (args {cmp_median=True}, t)))
      "Report the median over all runs of each command."
  , Option ['d'] ["diff"]
      (NoArg (\(args, t) -> (args {cmp_diff=True}, t)))
      "Report the difference in execution times between individual commands."
  ]

helpHeader cmd_name = "Usage: " ++ cmd_name ++ " [ OPTION... ]"

parseOpts :: [String] -> IO (Maybe Conf)
parseOpts argv = case getOpt Permute options argv of
        (o, n, []   ) | null n    -> res o
                      | otherwise -> do
                            hPutStrLn stderr $ "WARNING: Unparsed arguments: " ++ (show n)
                            res o
        (_, _, errs ) -> do
                      cmd_name <- getProgName
                      ioError $ userError $ concat errs ++ usageInfo (helpHeader cmd_name) options
    where
        res mods = case foldl (flip id) (defaultConf, Nothing) mods of
                 (conf, _) | help conf -> do
                                cmd_name <- getProgName
                                hPutStrLn stderr $ usageInfo (helpHeader cmd_name) options
                                return Nothing
                           | otherwise -> return $ Just conf {commands = (reverse $ commands conf)}

{- Process execution -}

newtype PrelExecResult = PrelExecResult [Double]

data ExecResult = ExecResult {
  cleanExecs :: Word32,

  stddev     :: Double,
  mean_µs    :: Double,
  geomean_µs :: Double,
  median_µs  :: Double
}

finalize :: PrelExecResult -> IO ExecResult
finalize (PrelExecResult times) = do
  times_mvec <- V.unsafeThaw $ V.fromList times
  Merge.sort times_mvec;
  times_vec <- V.unsafeFreeze times_mvec
  let numCleanExecs = fromIntegral $ V.length times_vec
      median = times_vec V.! ( fromIntegral $ numCleanExecs `div` 2)
      (avg, variance) = meanVarianceUnb times_vec
      geomean = geometricMean times_vec

  return $ ExecResult {
    cleanExecs = numCleanExecs,
    stddev     = sqrt variance,
    mean_µs    = avg,
    geomean_µs = geomean,
    median_µs  = median
  }

executeCommand :: Conf -> Word32 -> Word32 -> PrelExecResult -> Command -> IO PrelExecResult
executeCommand conf@Conf{times=maxn, chk_ret_code=chk, verbose=verb} i n (PrelExecResult times) (Cmd cmd args) = do
  absCmd <- if T.head cmd == '.'
            then shelly $ absPath $ fromText cmd
            else return $ fromText cmd
  let exec = errExit False $ time $ run_ absCmd args
  let cstr = show $ T.unwords $ cmd : args
  printf "\n"
  when (verb && n > 1) $ printf "────────────────────────────────\n"
  printf "  ⇶ Executing %s\n" cstr
  when (verb && n > 1) $ printf "┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈\n"
  ntimes :: [Double] <- foldM (\tms i -> do
      when (verb) $ printf "    [ %3d of %d ] " i maxn
      (time, ec) <- shelly $ do
        (tm, _) <- exec
        exit_code <- lastExitCode
        return (tm * 1e6, exit_code)
      if (ec == 0 || not chk)
      then do
        when verb $ printf "%.0fµs\n" $ time
        return (time : tms)
      else do
        when verb $ printf "Non-successful execution...\n"
        return tms
    ) times [i..n+i-1]
  
  return $ PrelExecResult ntimes

executeCommands :: Conf -> IO [ExecResult]
executeCommands conf@Conf{times=n, interleaved=interleaved}
  | interleaved = do
      let crPairs = zip (commands conf) (repeat (PrelExecResult []))
          cmdStep i (cmd, pr) = do
            npr <- executeCommand conf i 1 pr cmd
            return (cmd, npr)
          step prs i = mapM (cmdStep i) prs
      prelResults <- foldM step crPairs [1..n]
      mapM (finalize . snd) prelResults
  | otherwise = mapM (\cmd -> executeCommand conf 1 n (PrelExecResult []) cmd >>= finalize) $ commands conf

{- Summary printing -}

printDiff :: Conf -> ((Command, ExecResult), (Command, ExecResult)) -> IO ()
printDiff conf ((Cmd c1 a1, r1), (Cmd c2 a2, r2)) = do
  let cstr1 = show $ T.unwords $ c1 : a1
      cstr2 = show $ T.unwords $ c2 : a2
      stddev1 = stddev r1
      stddev2 = stddev r2
      relstddev1 = stddev1 * 100 / (mean_µs r1)
      relstddev2 = stddev2 * 100 / (mean_µs r2)
      relDiff field = 100 * ((field r2) / (field r1) -1)

  printf                         "\n  ┌     %s \n  │     stddev: %4.2fµs = %4.2f%% of mean\n" cstr1 stddev1 relstddev1
  printf                           "  ├ vs. %s \n  │     stddev: %4.2fµs = %4.2f%% of mean\n" cstr2 stddev2 relstddev2
  printf                           "  ├────────────────────────────────\n"
  when (cmp_mean conf)    $ printf "  │        mean: %4.2f%%\n" $ relDiff mean_µs
  when (cmp_geomean conf) $ printf "  │     geomean: %4.2f%%\n" $ relDiff geomean_µs
  when (cmp_median conf)  $ printf "  │      median: %4.2f%%\n" $ relDiff median_µs

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
      results <- executeCommands conf
      when (cmp_diff conf) $ printDiffs conf (commands conf) results
    Nothing -> return ()
