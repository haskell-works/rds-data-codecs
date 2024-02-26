{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module App.Cli.Run.BatchExecuteStatement
  ( runBatchExecuteStatementCmd
  ) where

import qualified App.Cli.Types as CLI

runBatchExecuteStatementCmd :: CLI.BatchExecuteStatementCmd -> IO ()
runBatchExecuteStatementCmd _ = pure ()
