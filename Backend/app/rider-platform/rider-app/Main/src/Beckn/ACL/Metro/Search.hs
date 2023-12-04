{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}

module Beckn.ACL.Metro.Search where

import qualified Beckn.Types.Core.Taxi.Common.Vehicle as Veh
import qualified Control.Lens as L
import Data.Aeson
import Data.OpenApi hiding (tags, value)
import qualified Data.Text as T
import Data.Typeable
import qualified Domain.Action.UI.TestMetro.Search as DMetroSearch
import EulerHS.Prelude hiding (fromList, (.~))
import GHC.Exts (IsList (fromList))
import Kernel.Prelude
import Kernel.Types.Beckn.Ack
import Kernel.Types.Beckn.Context hiding (Context)
import Kernel.Types.Beckn.Error (Error)
import Kernel.Types.Beckn.Gps
import Kernel.Types.Common
import Kernel.Utils.Common hiding (Error (..))
import Kernel.Utils.GenericPretty (PrettyShow)
import Kernel.Utils.JSON (stripPrefixUnderscoreIfAny)
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)
import Kernel.Utils.TH (mkHttpInstancesForEnum)
import Servant hiding (throwError)
import Tools.Beam.UtilsTH (mkBeamInstancesForEnum)
import qualified Tools.Maps as Maps

buildMetroSearchReq ::
  (MonadFlow m, HasFlowEnv m r '["nwAddress" ::: BaseUrl]) =>
  DMetroSearch.MetroSearchRes ->
  m MetroSearchReq
buildMetroSearchReq res@DMetroSearch.MetroSearchRes {..} = do
  bapUrl <- asks (.nwAddress) <&> #baseUrlPath L.%~ (<> "/" <> T.unpack merchant.id.getId)
  let context = buildMetroContext now bapUrl res
  message <- buildMetroMessage res
  pure $ BecknReq {context, message}

buildMetroContext :: UTCTime -> BaseUrl -> DMetroSearch.MetroSearchRes -> MetroContext
buildMetroContext now bapUrl res =
  let location = buildMetroContextLocation
   in MetroContext
        { location = location,
          domain = PUBLIC_TRANSPORT,
          timestamp = now,
          bap_id = res.merchant.bapId,
          transaction_id = res.searchId.getId,
          message_id = res.searchId.getId,
          core_version = "2.0.0",
          action = SEARCH,
          bap_uri = bapUrl,
          ttl = "PT30S", -- JAYPAL
          bpp_id = Nothing,
          bpp_uri = Nothing,
          max_callbacks = Nothing
        }
  where
    buildMetroContextLocation = ContextLocation {country = ContextLocationCode {code = India}, city = ContextLocationCode {code = Bangalore}}

buildMetroMessage :: (MonadFlow m) => DMetroSearch.MetroSearchRes -> m MetroSearchMessage
buildMetroMessage res = do
  fulfillment <- buildMetroFulfillment res
  let tags = buildMetroPaymentTags
      payment = MetroPayment {..}
      intent = MetroIntent {fulfillment, payment}
  pure $ MetroSearchMessage {..}

buildMetroFulfillment :: (MonadFlow m) => DMetroSearch.MetroSearchRes -> m MetroFulfillment
buildMetroFulfillment res = do
  let startStop = MetroStop {_type = START, location = toGps res.origin.gps}
      endStop = MetroStop {_type = END, location = toGps res.destination.gps}
      stops = [startStop, endStop]
      vehicle = Veh.Vehicle {category = Veh.METRO}
  pure $ MetroFulfillment {stops, vehicle}

buildMetroPaymentTags :: [MetroTagGroup]
buildMetroPaymentTags =
  let feePercentageDescriptor = MetroDescriptor {_code = BUYER_FINDER_FEES_PERCENTAGE}
      feePercentageTag = MetroTag {descriptor = feePercentageDescriptor, value = "1"}
      delayInterestDescriptor = MetroDescriptor {_code = DELAY_INTEREST}
      delayInterestTag = MetroTag {descriptor = delayInterestDescriptor, value = "2.5"}
      staticTermsDescriptor = MetroDescriptor {_code = STATIC_TERMS}
      staticTermsTag = MetroTag {descriptor = staticTermsDescriptor, value = "https://api.example-bap.com/booking/terms"}
      finderfeeDescriptor = MetroDescriptor {_code = BUYER_FINDER_FEES}
      finderFeeTagGroup = MetroTagGroup {descriptor = finderfeeDescriptor, display = False, list = [feePercentageTag]}
      settlementTermsDescriptor = MetroDescriptor {_code = SETTLEMENT_TERMS}
      settlementTermsTagGroup = MetroTagGroup {descriptor = settlementTermsDescriptor, display = False, list = [delayInterestTag, staticTermsTag]}
   in [finderFeeTagGroup, settlementTermsTagGroup]

type MetroSearchReq = BecknReq MetroSearchMessage

type MetroSearchAPI =
  "search"
    :> ReqBody '[JSON] MetroSearchReq
    :> Post '[JSON] AckResponse

metroSearchAPI :: Proxy MetroSearchAPI
metroSearchAPI = Proxy

data BecknReq a = BecknReq
  { context :: MetroContext,
    message :: a
  }
  deriving (Generic, Show, FromJSON, ToJSON, PrettyShow)

instance ToSchema a => ToSchema (BecknReq a)

data BecknCallbackReq a = BecknCallbackReq
  { context :: MetroContext,
    contents :: Either Error a
  }
  deriving (Generic, Show, PrettyShow)

instance (ToSchema a) => ToSchema (BecknCallbackReq a) where
  declareNamedSchema _ = do
    context <- declareSchemaRef (Proxy :: Proxy MetroContext)
    err <- declareSchemaRef (Proxy :: Proxy Error)
    let messageTypeName = show $ typeRep (Proxy :: Proxy a)
    message <- declareSchemaRef (Proxy :: Proxy a)
    let errVariant =
          Inline $
            mempty
              & type_ L.?~ OpenApiObject
              & properties L..~ fromList [("context", context), ("error", err)]
              & required L..~ ["context", "error"]
        messageVariant =
          Inline $
            mempty
              & type_ L.?~ OpenApiObject
              & properties L..~ fromList [("context", context), ("message", message)]
              & required L..~ ["context", "message"]
    return $
      NamedSchema (Just $ "BecknCallbackReq_" <> messageTypeName) $
        mempty
          & type_ L.?~ OpenApiObject
          & oneOf L.?~ [messageVariant, errVariant]

instance ToJSON a => ToJSON (BecknCallbackReq a) where
  toJSON (BecknCallbackReq context contents) = object $ contextField : errorOrMessage
    where
      contextField = "context" .= context
      errorOrMessage = case contents of
        Left err -> ["error" .= err]
        Right message -> ["message" .= message]

instance FromJSON a => FromJSON (BecknCallbackReq a) where
  parseJSON = withObject "BecknCallbackReq" $ \o ->
    BecknCallbackReq
      <$> o .: "context"
      <*> (Left <$> o .: "error" <|> Right <$> o .: "message")

data MetroContext = MetroContext
  { location :: ContextLocation,
    domain :: Domain,
    timestamp :: UTCTime,
    bap_id :: Text,
    transaction_id :: Text,
    message_id :: Text,
    core_version :: Text,
    action :: Action,
    bap_uri :: BaseUrl,
    ttl :: Text,
    bpp_id :: Maybe Text,
    bpp_uri :: Maybe BaseUrl,
    max_callbacks :: Maybe Int
  }
  deriving (Generic, FromJSON, Show, ToSchema, PrettyShow)

instance ToJSON MetroContext where
  toJSON = genericToJSON $ defaultOptions {omitNothingFields = True}

data ContextLocation = ContextLocation
  { country :: ContextLocationCode Country,
    city :: ContextLocationCode City
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema, PrettyShow)

newtype ContextLocationCode a = ContextLocationCode
  { code :: a
  }
  deriving (Generic, Show, PrettyShow)

instance (FromJSON a) => FromJSON (ContextLocationCode a) where
  parseJSON = genericParseJSON stripPrefixUnderscoreAndRemoveNullFields

instance (ToJSON a) => ToJSON (ContextLocationCode a) where
  toJSON = genericToJSON stripPrefixUnderscoreAndRemoveNullFields

instance (ToSchema a) => ToSchema (ContextLocationCode a) where
  declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions stripPrefixUnderscoreAndRemoveNullFields

newtype MetroSearchMessage = MetroSearchMessage
  { intent :: MetroIntent
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data MetroIntent = MetroIntent
  { fulfillment :: MetroFulfillment,
    payment :: MetroPayment
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data MetroFulfillment = MetroFulfillment
  { stops :: [MetroStop],
    vehicle :: MetroVehicle
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data MetroStop = MetroStop
  { _type :: StopType,
    location :: Gps
  }
  deriving (Generic, Show)

instance FromJSON MetroStop where
  parseJSON = genericParseJSON stripPrefixUnderscoreAndRemoveNullFields

instance ToJSON MetroStop where
  toJSON = genericToJSON stripPrefixUnderscoreAndRemoveNullFields

instance ToSchema MetroStop where
  declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions stripPrefixUnderscoreAndRemoveNullFields

type MetroVehicle = Veh.Vehicle

newtype MetroPayment = MetroPayment
  { tags :: [MetroTagGroup]
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data MetroTagGroup = MetroTagGroup
  { descriptor :: MetroDescriptor PaymentTagGroupCode,
    display :: Bool,
    list :: [MetroTag]
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data MetroTag = MetroTag
  { descriptor :: MetroDescriptor PaymentTagCode,
    value :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

newtype MetroDescriptor a = MetroDescriptor
  { _code :: a
  }
  deriving (Generic, Show, PrettyShow)

instance (FromJSON a) => FromJSON (MetroDescriptor a) where
  parseJSON = genericParseJSON stripPrefixUnderscoreAndRemoveNullFields

instance (ToJSON a) => ToJSON (MetroDescriptor a) where
  toJSON = genericToJSON stripPrefixUnderscoreAndRemoveNullFields

instance (ToSchema a) => ToSchema (MetroDescriptor a) where
  declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions stripPrefixUnderscoreAndRemoveNullFields

data PaymentTagGroupCode = BUYER_FINDER_FEES | SETTLEMENT_TERMS
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data PaymentTagCode = BUYER_FINDER_FEES_PERCENTAGE | DELAY_INTEREST | STATIC_TERMS
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

stripPrefixUnderscoreAndRemoveNullFields :: Options
stripPrefixUnderscoreAndRemoveNullFields =
  stripPrefixUnderscoreIfAny
    { omitNothingFields = True
    }

toGps :: Maps.LatLong -> Gps
toGps (Maps.LatLong lat lon) = Gps {lat, lon}

data StopType = START | END | INTERMEDIATE_STOP | TRANSIT_STOP
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

$(mkBeamInstancesForEnum ''PaymentTagGroupCode)
$(mkHttpInstancesForEnum ''PaymentTagGroupCode)

$(mkBeamInstancesForEnum ''PaymentTagCode)
$(mkHttpInstancesForEnum ''PaymentTagCode)

$(mkBeamInstancesForEnum ''StopType)
$(mkHttpInstancesForEnum ''StopType)
