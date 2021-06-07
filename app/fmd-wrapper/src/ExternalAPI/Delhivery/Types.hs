{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module ExternalAPI.Delhivery.Types where

import Beckn.Types.Error.APIError
import Beckn.Types.Error.FromResponse
import Data.Aeson hiding (Error)
import EulerHS.Prelude hiding (zip)
import Types.Common
import Web.FormUrlEncoded

data TokenReq = TokenReq
  { client_id :: ClientId,
    client_secret :: ClientSecret,
    grant_type :: Text
  }
  deriving (Generic, Show, ToForm)

data TokenRes = TokenRes
  { access_token :: Token,
    expires_in :: Integer,
    refresh_expires_in :: Integer,
    refresh_token :: Token,
    token_type :: Text,
    not_before_policy :: Maybe Integer,
    session_state :: Text,
    scope :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data CreateOrderReq = CreateOrderReq
  { inv :: Maybe Integer,
    itm :: [ItemDetails],
    oid :: Maybe Text,
    cod :: Maybe Integer,
    src :: LocationDetails,
    ret :: LocationDetails,
    tar :: LocationDetails
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data ItemDetails = ItemDetails
  { prd :: Text,
    qty :: Integer,
    inv :: Maybe Integer,
    cod :: Maybe Integer
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data LocationDetails = LocationDetails
  { pho :: Text,
    nam :: Text,
    eml :: Maybe Text,
    det :: Address
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data Address = Address
  { add :: Text,
    cty :: Text,
    reg :: Text,
    crd :: Text,
    cnt :: Maybe Text,
    zip :: Maybe Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

newtype Error = Error
  { message :: Text
  }
  deriving (Show, Generic)

instance FromResponse Error where
  fromResponse = fromJsonResponse

errMessageOptions :: Options
errMessageOptions =
  defaultOptions
    { fieldLabelModifier =
        let f "message" = "Message"
            f other = other
         in f
    }

instance FromJSON Error where
  parseJSON = genericParseJSON errMessageOptions

instance ToJSON Error where
  toJSON = genericToJSON errMessageOptions

data QuoteReq = QuoteReq
  { inv :: Maybe Integer,
    itm :: [ItemDetails],
    oid :: Maybe Text,
    cod :: Maybe Integer,
    src :: LocationDetails,
    tar :: LocationDetails
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data QuoteRes = QuoteRes
  { eta :: Integer,
    pricing :: Float,
    success :: Bool
  }
  deriving (Show, Generic, ToJSON, FromJSON)

newtype CreateOrderRes = CreateOrderRes
  { idx :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

instance IsAPIError Error where
  toErrorCode _ = "DELHIVERY_ERROR"
  toHttpCode _ = E500
  toMessage Error {message} = Just message

instanceExceptionWithParent 'APIException ''Error
