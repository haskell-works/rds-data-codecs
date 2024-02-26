{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module App.Cli.Types
  ( Cmd(..)
  , ExecuteStatementCmd(..)
  , BatchExecuteStatementCmd(..)
  , ExampleCmd(..)
  ) where

import Data.ByteString (ByteString)
import Data.RdsData.Types ()
import Data.Text
import GHC.Generics

import qualified Amazonka         as AWS
import qualified Amazonka.RDSData as AWS

data Cmd =
    CmdOfExecuteStatementCmd ExecuteStatementCmd
  | CmdOfBatchExecuteStatementCmd BatchExecuteStatementCmd
  | CmdOfExampleCmd ExampleCmd

data ExecuteStatementCmd = ExecuteStatementCmd
  { mAwsLogLevel  :: Maybe AWS.LogLevel
  , region        :: AWS.Region
  , mHostEndpoint :: Maybe (ByteString, Int, Bool)
  , resourceArn   :: Text
  , secretArn     :: Text
  , sql           :: Text
  } deriving Generic

data BatchExecuteStatementCmd = BatchExecuteStatementCmd
  { mAwsLogLevel  :: Maybe AWS.LogLevel
  , region        :: AWS.Region
  , mHostEndpoint :: Maybe (ByteString, Int, Bool)
  , parameterSets :: Maybe [[AWS.SqlParameter]]
  , resourceArn   :: Text
  , secretArn     :: Text
  , sql           :: Text
  } deriving Generic

data ExampleCmd = ExampleCmd
  { mAwsLogLevel  :: Maybe AWS.LogLevel
  , region        :: AWS.Region
  , mHostEndpoint :: Maybe (ByteString, Int, Bool)
  , resourceArn   :: Text
  , secretArn     :: Text
  } deriving Generic
