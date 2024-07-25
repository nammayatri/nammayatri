module Lib.Yudhishthira.Types.KaalChakra where

import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Lib.Yudhishthira.Types.Common

data NammaTagChakra = NammaTagChakra
  { tagCategory :: Text,
    tagName :: Text,
    tagPossibleValues :: [Text],
    tagChakra :: Chakra,
    tagValidity :: Maybe Hours,
    tagRule :: TagRule
  }
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data Chakra
  = Daily
  | Weekly
  | Monthly
  | Quaterly
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data KaalChakraRideData = KaalChakraRideData
  { rides :: Maybe Int,
    cancelledRide :: Maybe Int,
    completedRides :: Maybe Int,
    cancellationRate :: Maybe Int
  }
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

newtype KaalChakraSearchData = KaalChakraSearchData
  { acceptanceRate :: Maybe Int
  }
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data KaalChakraDataPoint = KaalChakraDataPoint
  { ride :: Maybe KaalChakraRideData,
    search :: Maybe KaalChakraSearchData
  }
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data User = Driver | Customer deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data KaalChakraData = KaalChakraData
  { userId :: Id User,
    daily :: Maybe KaalChakraDataPoint,
    weekly :: Maybe KaalChakraDataPoint,
    movingWeekly :: Maybe KaalChakraDataPoint,
    monthly :: Maybe KaalChakraDataPoint,
    movingMonthly :: Maybe KaalChakraDataPoint,
    total :: Maybe KaalChakraDataPoint
  }
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)
