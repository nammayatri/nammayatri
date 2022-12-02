{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.FarePolicy.FareProductType where

import Beckn.Prelude
import Beckn.Storage.Esqueleto

data FareProductType = ONE_WAY | RENTAL | DRIVER_OFFER deriving (Generic, Show, Read, Eq, FromJSON, ToJSON, ToSchema)

derivePersistField "FareProductType"
