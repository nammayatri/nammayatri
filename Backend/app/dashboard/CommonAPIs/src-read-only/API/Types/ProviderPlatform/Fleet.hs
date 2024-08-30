{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Fleet where

import qualified API.Types.ProviderPlatform.Fleet.Driver
import qualified Dashboard.Common
import qualified Data.List
import Data.OpenApi (ToSchema)
import EulerHS.Prelude
import qualified Text.Read
import qualified Text.Show

newtype FleetEndpoint
  = DriverAPI API.Types.ProviderPlatform.Fleet.Driver.DriverEndpointDSL
  deriving stock (Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Text.Show.Show FleetEndpoint where
  show = \case
    DriverAPI e -> "DRIVER/" <> Dashboard.Common.showUserActionType e

instance Text.Read.Read FleetEndpoint where
  readsPrec d' = Text.Read.readParen (d' > app_prec) (\r -> [(DriverAPI v1, r2) | r1 <- stripPrefix "DRIVER/" r, (v1, r2) <- Dashboard.Common.readUserActionTypeS r1])
    where
      app_prec = 10
      stripPrefix pref r = bool [] [Data.List.drop (length pref) r] $ Data.List.isPrefixOf pref r
