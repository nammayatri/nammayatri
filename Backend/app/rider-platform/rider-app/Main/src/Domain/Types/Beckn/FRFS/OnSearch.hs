module Domain.Types.Beckn.FRFS.OnSearch where

import qualified BecknV2.FRFS.Enums as Spec
import qualified Domain.Types.FRFSFarePolicy as FRFSFarePolicy
import qualified Domain.Types.FRFSQuote as Quote
import Domain.Types.FRFSQuoteCategoryType
import qualified Domain.Types.FRFSSearch as Search
import Domain.Types.Merchant
import qualified Domain.Types.StationType as Station
import Kernel.External.Maps.Types
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id

data DOnSearch = DOnSearch
  { bppSubscriberId :: Text,
    bppSubscriberUrl :: Text,
    providerDescription :: Maybe Text,
    providerId :: Text,
    providerName :: Text,
    quotes :: [DQuote],
    validTill :: Maybe UTCTime,
    transactionId :: Text,
    messageId :: Text,
    bppDelayedInterest :: Maybe Text
  }
  deriving (Show)

data DiscoveryOnSearchReq = DiscoveryOnSearchReq
  { transactionId :: Text,
    messageId :: Text,
    pageNumber :: Int,
    totalPages :: Int,
    bppSubscriberId :: Text,
    bppSubscriberUrl :: Text,
    stationList :: [DStation],
    merchantId :: Text
  }

data DiscoveryCounter = DiscoveryCounter
  { merchantId :: Text,
    pageNo :: Int,
    maxPageNo :: Int
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data DVehicleServiceTier = DVehicleServiceTier
  { serviceTierType :: Spec.ServiceTierType,
    serviceTierProviderCode :: Text,
    serviceTierShortName :: Text,
    serviceTierDescription :: Text,
    serviceTierLongName :: Text,
    isAirConditioned :: Maybe Bool
  }
  deriving (Show)

data DQuote = DQuote
  { bppItemId :: Text,
    routeCode :: Text,
    vehicleType :: Spec.VehicleCategory,
    routeStations :: [DRouteStation],
    stations :: [DStation],
    categories :: [DCategory],
    fareDetails :: Maybe Quote.FRFSFareDetails,
    _type :: Quote.FRFSQuoteType
  }
  deriving (Show)

data DCategory = DCategory
  { category :: FRFSQuoteCategoryType,
    price :: Price,
    offeredPrice :: Price,
    bppItemId :: Text,
    eligibility :: Bool
  }
  deriving (Show)

data DRouteStation = DRouteStation
  { routeCode :: Text,
    routeLongName :: Text,
    routeShortName :: Text,
    routeStartPoint :: LatLong,
    routeEndPoint :: LatLong,
    routeStations :: [DStation],
    routeTravelTime :: Maybe Seconds,
    routeSequenceNum :: Maybe Int,
    routeServiceTier :: Maybe DVehicleServiceTier,
    routePrice :: Price,
    routeColor :: Maybe Text,
    routeFarePolicyId :: Maybe (Id FRFSFarePolicy.FRFSFarePolicy)
  }
  deriving (Show)

data DStation = DStation
  { stationCode :: Text,
    stationName :: Text,
    stationLat :: Maybe Double,
    stationLon :: Maybe Double,
    stationType :: Station.StationType,
    stopSequence :: Maybe Int,
    towards :: Maybe Text
  }
  deriving (Show)

data ValidatedDOnSearch = ValidatedDOnSearch
  { merchant :: Merchant,
    search :: Search.FRFSSearch,
    ticketsBookedInEvent :: Int,
    isEventOngoing :: Bool,
    mbFreeTicketInterval :: Maybe Int,
    mbMaxFreeTicketCashback :: Maybe Int
  }
  deriving (Show)
