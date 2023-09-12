{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Missing NOINLINE pragma" #-}

module Lib.Schema where

import Environment (AppCfg)
import Kernel.Prelude
import Kernel.Utils.Dhall
import System.IO.Unsafe (unsafePerformIO)

appCfg :: AppCfg
appCfg = unsafePerformIO $ readDhallConfigDefault "dynamic-offer-driver-app"

getLocationSchemaName :: Text
getLocationSchemaName = appCfg.esqLocationDBCfg.connectSchemaName
