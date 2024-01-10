{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Utils.Types where

import Kernel.Prelude
import Kernel.Utils.Dhall (FromDhall)

data AlchemistConfigs = AlchemistConfigs
  { riderApp :: AppConfigs,
    driverApp :: AppConfigs
  }
  deriving (Generic, Show, FromDhall)

data AppConfigs = AppConfigs
  { inputFileConfigs :: InputYamlFilePaths,
    outputFileConfigs :: OutputYamlFilePaths
  }
  deriving (Generic, Show, FromDhall)

data InputYamlFilePaths = InputYamlFilePaths
  { apiYaml :: FilePath,
    storageYaml :: FilePath
  }
  deriving (Generic, Show, FromDhall)

data OutputYamlFilePaths = OutputYamlFilePaths
  { apiTypesOutputFilePath :: FilePath,
    beamQueriesOutputFilePath :: FilePath,
    beamTableOutputFilePath :: FilePath,
    domainHandlerOutputFilePath :: FilePath,
    domainTypeOutputFilePath :: FilePath,
    servantAPIOutputFilePath :: FilePath,
    sqlOutputFilePath :: FilePath
  }
  deriving (Generic, Show, FromDhall)
