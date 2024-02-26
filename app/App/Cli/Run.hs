{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module App.Cli.Run
  ( runCmd
  ) where

import App.Cli.Run.Example
import App.Cli.Run.ExecuteStatement

import qualified App.Cli.Types as CLI

runCmd :: CLI.Cmd -> IO ()
runCmd = \case
  CLI.CmdOfExecuteStatementCmd cmd ->
    runExecuteStatementCmd cmd
  CLI.CmdOfBatchExecuteStatementCmd cmd ->
    runBatchExecuteStatementCmd cmd
  CLI.CmdOfExampleCmd cmd ->
    runExampleCmd cmd

runBatchExecuteStatementCmd :: CLI.BatchExecuteStatementCmd -> IO ()
runBatchExecuteStatementCmd _ = pure ()
