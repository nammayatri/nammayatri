{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.RideBooking where

import qualified API.Types.ProviderPlatform.RideBooking.Driver
import qualified Dashboard.Common
import qualified Data.List
import Data.OpenApi (ToSchema)
import EulerHS.Prelude
import qualified Text.Read
import qualified Text.Show

newtype RideBookingEndpoint
  = DriverAPI API.Types.ProviderPlatform.RideBooking.Driver.DriverEndpointDSL
  deriving stock (Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Text.Show.Show RideBookingEndpoint where
  show = \case
    DriverAPI e -> "DRIVER/" <> Dashboard.Common.showUserActionType e

instance Text.Read.Read RideBookingEndpoint where
  readsPrec d' = Text.Read.readParen (d' > app_prec) (\r -> [(DriverAPI v1, r2) | r1 <- stripPrefix "DRIVER/" r, (v1, r2) <- Dashboard.Common.readUserActionTypeS r1])
    where
      app_prec = 10
      stripPrefix pref r = bool [] [Data.List.drop (length pref) r] $ Data.List.isPrefixOf pref r
