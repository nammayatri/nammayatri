{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.Search where

import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified Domain.Types.LocationAddress
import qualified Domain.Types.RefereeLink
import qualified Domain.Types.SearchRequest
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Maps.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import Tools.Auth
import qualified Tools.Maps

data SearchReqLocation = SearchReqLocation {address :: Domain.Types.LocationAddress.LocationAddress, gps :: Kernel.External.Maps.Types.LatLong}
  deriving stock (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SearchReqV2 = SearchReqV2
  { driverIdentifier :: Kernel.Prelude.Maybe Domain.Types.RefereeLink.DriverIdentifier,
    estimatedRentalDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
    estimatedRentalDuration :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    isDestinationManuallyMoved :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isReallocationEnabled :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isSourceManuallyMoved :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isSpecialLocation :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    origin :: SearchReqLocation,
    placeNameSource :: Kernel.Prelude.Maybe Data.Text.Text,
    quotesUnifiedFlow :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    returnTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    riderPreferredOption :: Domain.Types.SearchRequest.RiderPreferredOption,
    roundTrip :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    sessionToken :: Kernel.Prelude.Maybe Data.Text.Text,
    startTime :: Kernel.Prelude.UTCTime,
    stops :: [SearchReqLocation]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SearchResp = SearchResp {routeInfo :: Kernel.Prelude.Maybe Tools.Maps.RouteInfo, searchExpiry :: Kernel.Prelude.UTCTime, searchId :: Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest}
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
