{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Dashboard.RiderPlatform.Management.Ride where

import API.Types.RiderPlatform.Management.Endpoints.Ride
import Data.Aeson
import Kernel.Prelude (identity)
import Kernel.Storage.Esqueleto (derivePersistField)
import Kernel.Types.HideSecrets
import Kernel.Utils.JSON (constructorsWithLowerCase)
import Kernel.Utils.TH (mkHttpInstancesForEnum)

derivePersistField "BookingStatus"

$(mkHttpInstancesForEnum ''BookingStatus)

deriving anyclass instance FromJSON TicketRideListRes

deriving anyclass instance ToJSON TicketRideListRes

instance HideSecrets TicketRideListRes where
  hideSecrets = identity

instance FromJSON RideInfo where
  parseJSON = genericParseJSON constructorsWithLowerCase

instance ToJSON RideInfo where
  toJSON = genericToJSON constructorsWithLowerCase
