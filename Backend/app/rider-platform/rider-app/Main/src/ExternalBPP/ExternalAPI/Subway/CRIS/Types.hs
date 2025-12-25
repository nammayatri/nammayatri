module ExternalBPP.ExternalAPI.Subway.CRIS.Types where

import Kernel.Prelude
import qualified SharedLogic.FRFSUtils as FRFSUtils

data EncryptedResponse = EncryptedResponse
  { responseCode :: Text,
    responseData :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)

-- Request type with updated fields
data CRISFareRequest = CRISFareRequest
  { mobileNo :: Maybe Text,
    imeiNo :: Text,
    appSession :: Int,
    sourceCode :: Text,
    changeOver :: Text,
    rawChangeOver :: Text,
    destCode :: Text,
    via :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

-- Response types
data CRISFareResponse = CRISFareResponse
  { routeFareDetailsList :: [RouteFareDetails],
    sdkData :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data RouteFareDetails = RouteFareDetails
  { routeId :: Int,
    fareDtlsList :: [FareDetails],
    maximumValuesList :: [MaximumValues],
    allowedValuesList :: [AllowedValues]
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data FareDetails = FareDetails
  { adultFare :: Text,
    childFare :: Text,
    distance :: Int,
    via :: Text, -- Added via field
    ticketTypeCode :: Text,
    trainTypeCode :: Text,
    classCode :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data MaximumValues = MaximumValues
  { item :: Text,
    value :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data AllowedValues = AllowedValues
  { ticketTypeCode :: Text,
    ticketTypeName :: Text,
    trainTypeCode :: Text,
    trainTypeDescription :: Text,
    classCode :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data FRFSFareWithVia = FRFSFareWithVia
  { via :: Text,
    fares :: [FRFSUtils.FRFSFare]
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
