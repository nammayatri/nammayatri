{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Missing NOINLINE pragma" #-}
module Lib.Schema where

import Environment (AppCfg)
import Kernel.Prelude
import Kernel.Utils.Dhall
import System.IO.Unsafe (unsafePerformIO)

appCfg :: AppCfg
appCfg = unsafePerformIO $ readDhallConfigDefault "dynamic-offer-driver-app"

getDriverSchemaName :: Text
getDriverSchemaName = appCfg.esqDBCfg.connectSchemaName

getLocationSchemaName :: Text
getLocationSchemaName = appCfg.esqLocationDBCfg.connectSchemaName
