module Domain.Types.Ride where

import Beckn.Prelude
import Beckn.Types.Id
import qualified Domain.Types.DriverQuote as DQuote
import qualified Domain.Types.Organization as DOrg
import Domain.Types.Person
import qualified Domain.Types.SearchReqLocation as DLoc
import qualified Domain.Types.SearchRequest as DSR

data RideStatus = Active | Inactive

data Ride = Ride
  { id :: Id Ride,
    transactionId :: Text,
    requestId :: Id DSR.SearchRequest,
    quoteId :: Id DQuote.DriverQuote,
    status :: RideStatus,
    driverId :: Id Person,
    providerId :: Id DOrg.Organization,
    bapId :: Text,
    bapUri :: BaseUrl,
    startTime :: UTCTime,
    otp :: Text,
    fromLocationId :: Id DLoc.SearchReqLocation,
    baseFare :: Double,
    extraFareSelected :: Maybe Double,
    traveledDistance :: Double,
    chargeableDistance :: Maybe Double,
    trackingUrl :: Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
