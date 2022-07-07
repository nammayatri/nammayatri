{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Types.Common (module Reexport, module Types.Common) where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Common as Reexport

data FareProductType = ONE_WAY | RENTAL | AUTO deriving (Generic, Show, Read, Eq, FromJSON, ToJSON, ToSchema)

derivePersistField "FareProductType"

data BPPQuote
