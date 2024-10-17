{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module ExternalBPP.CMRL.ExternalAPI.PassengerViewStatus where

import Data.Aeson
import GHC.Generics (Generic)
import qualified Data.Text as T
import Domain.Types.IntegratedBPPConfig
import EulerHS.Types as ET
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Servant
import Tools.Error

-- Request body for Passenger View Status
data PassengerViewStatusReq = PassengerViewStatusReq
  { mobileNumber :: T.Text
  }
  deriving (Generic, Show)

instance ToJSON PassengerViewStatusReq where
  toJSON = genericToJSON defaultOptions

-- Response model for individual ticket details
data TicketDetails = TicketDetails
  { sourceStationCode :: T.Text,
    destinationStationCode :: T.Text,
    sourceStationName :: T.Text,
    destinationStationName :: T.Text,
    sourceStationTamilName :: Maybe T.Text,
    destinationStationTamilName :: Maybe T.Text,
    transactionTypeId :: T.Text,
    ticketReferenceNo :: T.Text,
    ticketStatus :: T.Text,
    originalValue :: Int,
    tranValue :: Int,
    txnRefNo :: T.Text,
    bankTxnRefNo :: T.Text,
    transactionTs :: T.Text,
    createdTs :: T.Text
  }
  deriving (Generic, Show)

instance FromJSON TicketDetails where
  parseJSON = genericParseJSON defaultOptions

-- Complete response model for Passenger View Status API
data PassengerViewStatusRes = PassengerViewStatusRes
  { statusCode :: Int,
    message :: T.Text,
    result :: [TicketDetails]
  }
  deriving (Generic, Show)

instance FromJSON PassengerViewStatusRes where
  parseJSON = genericParseJSON defaultOptions

-- Defining the API endpoint for Passenger View Status
type PassengerViewStatusAPI =
  "cumta" :> "passengerviewstatus"
    :> Header "Authorization" T.Text
    :> ReqBody '[JSON] PassengerViewStatusReq
    :> Get '[JSON] PassengerViewStatusRes

passengerViewStatusAPI :: Proxy PassengerViewStatusAPI
passengerViewStatusAPI = Proxy

-- Function to call the Passenger View Status API and return only the 'result' field (list of ticket details)
getPassengerViewStatus :: (CoreMetrics m, MonadFlow m) => T.Text -> T.Text -> PassengerViewStatusReq -> m [TicketDetails]
getPassengerViewStatus host accessToken req = do
  response <- callAPI host (ET.client passengerViewStatusAPI (Just $ "Bearer " <> accessToken) req) "getPassengerViewStatus" passengerViewStatusAPI
    >>= fromEitherM (ExternalAPICallError (Just "CMRL_PASSENGER_VIEW_STATUS_API") host)
  return response.result
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module ExternalBPP.CMRL.ExternalAPI.PassengerViewStatus where

import Data.Aeson
import GHC.Generics (Generic)
import qualified Data.Text as T
import Domain.Types.IntegratedBPPConfig
import EulerHS.Types as ET
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Servant
import Tools.Error

-- Request body for Passenger View Status
data PassengerViewStatusReq = PassengerViewStatusReq
  { mobileNumber :: T.Text
  }
  deriving (Generic, Show)

instance ToJSON PassengerViewStatusReq where
  toJSON = genericToJSON defaultOptions

-- Response model for individual ticket details
data TicketDetails = TicketDetails
  { sourceStationCode :: T.Text,
    destinationStationCode :: T.Text,
    sourceStationName :: T.Text,
    destinationStationName :: T.Text,
    sourceStationTamilName :: Maybe T.Text,
    destinationStationTamilName :: Maybe T.Text,
    transactionTypeId :: T.Text,
    ticketReferenceNo :: T.Text,
    ticketStatus :: T.Text,
    originalValue :: Int,
    tranValue :: Int,
    txnRefNo :: T.Text,
    bankTxnRefNo :: T.Text,
    transactionTs :: T.Text,
    createdTs :: T.Text
  }
  deriving (Generic, Show)

instance FromJSON TicketDetails where
  parseJSON = genericParseJSON defaultOptions

-- Complete response model for Passenger View Status API
data PassengerViewStatusRes = PassengerViewStatusRes
  { statusCode :: Int,
    message :: T.Text,
    result :: [TicketDetails]
  }
  deriving (Generic, Show)

instance FromJSON PassengerViewStatusRes where
  parseJSON = genericParseJSON defaultOptions

-- Defining the API endpoint for Passenger View Status
type PassengerViewStatusAPI =
  "cumta" :> "passengerviewstatus"
    :> Header "Authorization" T.Text
    :> ReqBody '[JSON] PassengerViewStatusReq
    :> Get '[JSON] PassengerViewStatusRes

passengerViewStatusAPI :: Proxy PassengerViewStatusAPI
passengerViewStatusAPI = Proxy

-- Function to call the Passenger View Status API and return only the 'result' field (list of ticket details)
getPassengerViewStatus :: (CoreMetrics m, MonadFlow m) => T.Text -> T.Text -> PassengerViewStatusReq -> m [TicketDetails]
getPassengerViewStatus host accessToken req = do
  response <- callAPI host (ET.client passengerViewStatusAPI (Just $ "Bearer " <> accessToken) req) "getPassengerViewStatus" passengerViewStatusAPI
    >>= fromEitherM (ExternalAPICallError (Just "CMRL_PASSENGER_VIEW_STATUS_API") host)
  return response.result
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module ExternalBPP.CMRL.ExternalAPI.PassengerViewStatus where

import Data.Aeson
import GHC.Generics (Generic)
import qualified Data.Text as T
import Domain.Types.IntegratedBPPConfig
import EulerHS.Types as ET
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Servant
import Tools.Error

data PassengerViewStatusReq = PassengerViewStatusReq
  { mobileNumber :: T.Text
  }
  deriving (Generic, Show)

instance ToJSON PassengerViewStatusReq where
  toJSON = genericToJSON defaultOptions

data TicketDetails = TicketDetails
  { sourceStationCode :: T.Text,
    destinationStationCode :: T.Text,
    sourceStationName :: T.Text,
    destinationStationName :: T.Text,
    sourceStationTamilName :: Maybe T.Text,
    destinationStationTamilName :: Maybe T.Text,
    transactionTypeId :: T.Text,
    ticketReferenceNo :: T.Text,
    ticketStatus :: T.Text,
    originalValue :: Int,
    tranValue :: Int,
    txnRefNo :: T.Text,
    bankTxnRefNo :: T.Text,
    transactionTs :: T.Text,
    createdTs :: T.Text
  }
  deriving (Generic, Show)

instance FromJSON TicketDetails where
  parseJSON = genericParseJSON defaultOptions

data PassengerViewStatusRes = PassengerViewStatusRes
  { statusCode :: Int,
    message :: T.Text,
    result :: [TicketDetails]
  }
  deriving (Generic, Show)

instance FromJSON PassengerViewStatusRes where
  parseJSON = genericParseJSON defaultOptions

type PassengerViewStatusAPI =
  "cumta" :> "passengerviewstatus"
    :> Header "Authorization" T.Text
    :> ReqBody '[JSON] PassengerViewStatusReq
    :> Get '[JSON] PassengerViewStatusRes

passengerViewStatusAPI :: Proxy PassengerViewStatusAPI
passengerViewStatusAPI = Proxy

getPassengerViewStatus :: (CoreMetrics m, MonadFlow m) => T.Text -> T.Text -> PassengerViewStatusReq -> m [TicketDetails]
getPassengerViewStatus host accessToken req = do
  response <- callAPI host (ET.client passengerViewStatusAPI (Just $ "Bearer " <> accessToken) req) "getPassengerViewStatus" passengerViewStatusAPI
    >>= fromEitherM (ExternalAPICallError (Just "CMRL_PASSENGER_VIEW_STATUS_API") host)
  return response.result
