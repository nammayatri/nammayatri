module Beckn.Exit where

import System.Exit (ExitCode (..))

exitSuccess :: ExitCode
exitSuccess = ExitSuccess

exitAuthManagerPrepFailure :: ExitCode
exitAuthManagerPrepFailure = ExitFailure 1

exitDBConnPrepFailure :: ExitCode
exitDBConnPrepFailure = ExitFailure 2

exitDBMigrationFailure :: ExitCode
exitDBMigrationFailure = ExitFailure 3

exitLoadAllProvidersFailure :: ExitCode
exitLoadAllProvidersFailure = ExitFailure 4

exitRedisConnPrepFailure :: ExitCode
exitRedisConnPrepFailure = ExitFailure 5

exitSigTERM :: ExitCode
exitSigTERM = ExitFailure 6

exitSigINT :: ExitCode
exitSigINT = ExitFailure 7

exitConnCheckFailure :: ExitCode
exitConnCheckFailure = ExitFailure 8
