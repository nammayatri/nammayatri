# CMRL v2 API Integration Implementation Plan

This plan outlines the integration of new CMRL v2 APIs while maintaining backward compatibility with existing v1 APIs.

## User Review Required

> [!IMPORTANT]
> **API Version Strategy**: This implementation creates a separate v2 folder structure alongside the existing v1 APIs. The system will support both versions simultaneously, allowing for gradual migration.

> [!IMPORTANT]
> **Configuration Changes**: A new `CMRLV2Config` will be added to `IntegratedBPPConfig`. You'll need to configure:
> - `operatorNameId` - Operator identifier for CMRL
> - `merchantId` - Merchant identifier
> - `encKeyIndex` - Encryption key index for ticket generation
> - Existing fields: `networkHostUrl`, `username`, `password`

> [!WARNING]
> **Breaking Change**: The ticket generation API now requires AES_CBC_PKCS5 encryption. The request payload must be encrypted before sending and responses must be decrypted.

## Proposed Changes

### Component 1: New CMRL v2 API Modules

Creating a new v2 directory structure to house all updated API implementations.

#### [NEW] Auth.hs

**Path**: `Backend/app/rider-platform/rider-app/Main/src/ExternalBPP/ExternalAPI/Metro/CMRL/v2/Auth.hs`

**Purpose**: Handle authentication with new v2 endpoint

**Key Changes from v1**:
- Endpoint: `/api/qr/v1/connect/token` (v1 was `/CmrlThirdParty/authenticate`)
- Request includes `operatorNameId`, `merchantId`, and `grantType` fields
- Response structure changed: includes `access_token`, `expires_in`, `token_type`, `refresh_token`, `key_index`, `key`, and `algo` (for encryption)

**Implementation**:
```haskell
{-# LANGUAGE OverloadedLists #-}

module ExternalBPP.ExternalAPI.Metro.CMRL.v2.Auth where

import Data.Aeson
import Domain.Types.Extra.IntegratedBPPConfig
import EulerHS.Types as ET
import ExternalBPP.ExternalAPI.Metro.CMRL.v2.Error
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.App
import Kernel.Utils.Common
import Kernel.Utils.Monitoring.Prometheus.Servant
import Servant hiding (throwError)
import Tools.Error

data AuthReq = AuthReq
  { operatorNameId :: Int,
    username :: Text,
    password :: Text,
    grantType :: Text,
    merchantId :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data AuthRes = AuthRes
  { access_token :: Text,
    expires_in :: Int,
    token_type :: Text,
    refresh_token :: Text,
    key_index :: Int,
    key :: Text,
    algo :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

type AuthAPI =
  "api" :> "qr" :> "v1" :> "connect" :> "token"
    :> ReqBody '[JSON] AuthReq
    :> Post '[JSON] AuthRes

authAPI :: Proxy AuthAPI
authAPI = Proxy

authTokenKey :: Text -> Text
authTokenKey merchantId = "CMRLV2Auth:Token:" <> merchantId

getAuthToken :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r, HasRequestId r, MonadReader r m) => CMRLV2Config -> m Text
getAuthToken config = do
  authToken :: (Maybe Text) <- Hedis.get (authTokenKey config.merchantId)
  case authToken of
    Nothing -> resetAuthToken config
    Just token -> return token

resetAuthToken :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r, HasRequestId r, MonadReader r m) => CMRLV2Config -> m Text
resetAuthToken config = do
  password <- decrypt config.password
  auth <-
    callAPI config.networkHostUrl (ET.client authAPI $ AuthReq config.operatorNameId config.username password "password" config.merchantId) "authCMRLV2" authAPI
      >>= fromEitherM (ExternalAPICallError (Just "CMRL_V2_AUTH_API") config.networkHostUrl)
  Hedis.setExp (authTokenKey config.merchantId) auth.access_token (2 * 3600)
  return auth.access_token

callCMRLV2API ::
  ( HasCallStack,
    CoreMetrics m,
    SanitizedUrl api,
    MonadFlow m,
    ToJSON res,
    CacheFlow m r,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  CMRLV2Config ->
  (Text -> ET.EulerClient res) ->
  Text ->
  Proxy api ->
  m res
callCMRLV2API config eulerClientFunc description proxy = do
  token <- getAuthToken config
  eitherResp <- withTryCatch "CMRLV2:auth" $ callApiUnwrappingApiError (identity @CMRLV2Error) Nothing Nothing Nothing config.networkHostUrl (eulerClientFunc token) description proxy
  case eitherResp of
    Left exec -> do
      let mbError = fromException @CMRLV2Error exec
          errorCode = mbError <&> toErrorCode
      case errorCode of
        Just "UNAUTHORIZED" -> do
          void $ resetAuthToken config
          callCMRLV2API config eulerClientFunc description proxy
        _ -> do
          case mbError of
            Just err -> throwError err
            Nothing -> throwError $ InternalError "CMRL V2 API Failed"
    Right resp -> return resp
```

---

#### [NEW] StationList.hs

**Path**: `Backend/app/rider-platform/rider-app/Main/src/ExternalBPP/ExternalAPI/Metro/CMRL/v2/StationList.hs`

**Purpose**: Fetch list of metro stations

**Key Changes from v1**:
- Endpoint: `/api/qr/v1/stations/list` (v1 was `/CmrlThirdParty/stations`)
- Query parameter: `operatorNameId` (required)
- Authorization header format: `Bearer {token}`

**Implementation**:
```haskell
{-# LANGUAGE OverloadedLists #-}

module ExternalBPP.ExternalAPI.Metro.CMRL.v2.StationList where

import Data.Aeson
import Domain.Types.Extra.IntegratedBPPConfig
import EulerHS.Types as ET
import ExternalBPP.ExternalAPI.Metro.CMRL.v2.Auth
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.App
import Kernel.Utils.Common
import Servant

data Station = Station
  { stationId :: Text,
    stationCode :: Text,
    stationName :: Text,
    latitude :: Maybe Double,
    longitude :: Maybe Double
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

data StationListResponse = StationListResponse
  { statusCode :: Int,
    message :: Text,
    result :: [Station]
  }
  deriving (Generic, Show, ToJSON, FromJSON)

type StationListAPI =
  "api" :> "qr" :> "v1" :> "stations" :> "list"
    :> Header "Authorization" Text
    :> QueryParam' '[Required, Strict] "operatorNameId" Int
    :> Get '[JSON] StationListResponse

stationListAPI :: Proxy StationListAPI
stationListAPI = Proxy

getStationList :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r, HasRequestId r, MonadReader r m) => CMRLV2Config -> m [Station]
getStationList config = do
  let eulerClient = \accessToken -> ET.client stationListAPI (Just $ "Bearer " <> accessToken) config.operatorNameId
  response <- callCMRLV2API config eulerClient "getStationList" stationListAPI
  return response.result
```

---

#### ~~[REMOVED] TicketType.hs~~

> [!NOTE]
> **This API is NOT needed** per code review. Ticket types are predefined:
> - SJT (Single Journey Ticket) - ID: 1
> - RJT (Return Journey Ticket) - ID: 102
> - GROUP - ID: 104
> - FAMILY - ID: 105
> - SVP (Stored Value Pass) - ID: 103
>
> The `ticketTypeId` and `fareTypeId` are now part of `CMRLV2Config` instead.

---

#### [UPDATED] Configuration Changes

**Path**: `Backend/app/rider-platform/rider-app/Main/src/ExternalBPP/ExternalAPI/Metro/CMRL/v2/TicketType.hs`

**Purpose**: Fetch available ticket types (NEW in v2)

**Implementation**:
```haskell
{-# LANGUAGE OverloadedLists #-}

module ExternalBPP.ExternalAPI.Metro.CMRL.v2.TicketType where

import Data.Aeson
import Domain.Types.Extra.IntegratedBPPConfig
import EulerHS.Types as ET
import ExternalBPP.ExternalAPI.Metro.CMRL.v2.Auth
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.App
import Kernel.Utils.Common
import Servant

data TicketType = TicketType
  { ticketTypeId :: Int,
    ticketTypeName :: Text,
    description :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data TicketTypeResponse = TicketTypeResponse
  { statusCode :: Int,
    message :: Text,
    result :: [TicketType]
  }
  deriving (Generic, Show, ToJSON, FromJSON)

type TicketTypeAPI =
  "api" :> "qr" :> "v1" :> "tickets" :> "ticket-types"
    :> Header "Authorization" Text
    :> QueryParam' '[Required, Strict] "operatorNameId" Int
    :> QueryParam' '[Required, Strict] "merchantId" Text
    :> Get '[JSON] TicketTypeResponse

ticketTypeAPI :: Proxy TicketTypeAPI
ticketTypeAPI = Proxy

getTicketTypes :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r, HasRequestId r, MonadReader r m) => CMRLV2Config -> m [TicketType]
getTicketTypes config = do
  let eulerClient = \accessToken -> ET.client ticketTypeAPI (Just $ "Bearer " <> accessToken) config.operatorNameId config.merchantId
  response <- callCMRLV2API config eulerClient "getTicketTypes" ticketTypeAPI
  return response.result
```

---

#### [NEW] BusinessHour.hs

**Path**: `Backend/app/rider-platform/rider-app/Main/src/ExternalBPP/ExternalAPI/Metro/CMRL/v2/BusinessHour.hs`

**Purpose**: Get business date and operation hours

**Key Changes from v1**:
- Endpoint: `/api/qr/v1/operations/business-date-hour` (v1 was `/CmrlThirdParty/businesshour`)
- Query parameter: `operatorNameId`

**Implementation**:
```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ExternalBPP.ExternalAPI.Metro.CMRL.v2.BusinessHour where

import Data.Aeson
import qualified Data.Text as T
import Domain.Types.Extra.IntegratedBPPConfig
import EulerHS.Types as ET
import ExternalBPP.ExternalAPI.Metro.CMRL.v2.Auth
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.App
import Kernel.Utils.Common
import Servant

data BusinessHourResult = BusinessHourResult
  { businessDate :: T.Text,
    operationStartTime :: T.Text,
    operationEndTime :: T.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data BusinessHourRes = BusinessHourRes
  { statusCode :: Int,
    message :: T.Text,
    result :: BusinessHourResult
  }
  deriving (Generic, Show, ToJSON, FromJSON)

type BusinessHourAPI =
  "api" :> "qr" :> "v1" :> "operations" :> "business-date-hour"
    :> Header "Authorization" T.Text
    :> QueryParam' '[Required, Strict] "operatorNameId" Int
    :> Get '[JSON] BusinessHourRes

businessHourAPI :: Proxy BusinessHourAPI
businessHourAPI = Proxy

getBusinessHour :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r, HasRequestId r, MonadReader r m) => CMRLV2Config -> m BusinessHourResult
getBusinessHour config = do
  let eulerClient = \accessToken -> ET.client businessHourAPI (Just $ "Bearer " <> accessToken) config.operatorNameId
  response <- callCMRLV2API config eulerClient "getBusinessHour" businessHourAPI
  return response.result
```

---

#### [NEW] GetFare.hs

**Path**: `Backend/app/rider-platform/rider-app/Main/src/ExternalBPP/ExternalAPI/Metro/CMRL/v2/GetFare.hs`

**Purpose**: Calculate fare between stations

**Key Changes from v1**:
- Endpoint: `/api/qr/v1/fare/getfare` (v1 was `/CmrlThirdParty/farebyod`)
- POST request instead of GET
- Includes `ticketTypeId`, `fareTypeId`, and `travelDatetime`

**Implementation**:
```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ExternalBPP.ExternalAPI.Metro.CMRL.v2.GetFare where

import qualified BecknV2.FRFS.Enums as Spec
import BecknV2.FRFS.Utils
import Data.Aeson
import qualified Data.Text as T
import Domain.Types.FRFSQuoteCategorySpec
import Domain.Types.FRFSQuoteCategoryType
import Domain.Types.IntegratedBPPConfig
import Domain.Types.Extra.IntegratedBPPConfig
import EulerHS.Types as ET
import ExternalBPP.ExternalAPI.Metro.CMRL.v2.Auth
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.App
import Kernel.Utils.Common
import Servant
import qualified SharedLogic.FRFSUtils as FRFSUtils
import qualified Storage.Queries.FRFSTicketCategoryMetadataConfig as QFRFSTicketCategoryMetadataConfig

data GetFareReq = GetFareReq
  { operatorNameId :: Int,
    fromStationId :: T.Text,
    toStationId :: T.Text,
    ticketTypeId :: Int,
    merchantId :: T.Text,
    travelDatetime :: T.Text,
    fareTypeId :: Int
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data GetFareResult = GetFareResult
  { fare :: Maybe HighPrecMoney,
    discountedFare :: Maybe HighPrecMoney
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data GetFareRes = GetFareRes
  { statusCode :: Int,
    message :: T.Text,
    result :: Maybe GetFareResult
  }
  deriving (Generic, Show, ToJSON, FromJSON)

type GetFareAPI =
  "api" :> "qr" :> "v1" :> "fare" :> "getfare"
    :> Header "Authorization" T.Text
    :> ReqBody '[JSON] GetFareReq
    :> Post '[JSON] GetFareRes

getFareAPI :: Proxy GetFareAPI
getFareAPI = Proxy

getFare :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r, EsqDBFlow m r) => IntegratedBPPConfig -> CMRLV2Config -> GetFareReq -> m [FRFSUtils.FRFSFare]
getFare integrationBPPConfig config fareReq = do
  let cacheKey = "cmrlv2-fare-" <> T.pack (show fareReq.operatorNameId) <> "-" <> fareReq.fromStationId <> "-" <> fareReq.toStationId <> "-" <> T.pack (show fareReq.ticketTypeId)
  mbCachedFares <- Hedis.get cacheKey
  case mbCachedFares of
    Just cachedFares -> do
      logDebug $ "Retrieved fares from cache for key: " <> cacheKey
      return cachedFares
    Nothing -> do
      logDebug $ "Cache miss for key: " <> cacheKey <> ", fetching from API"
      let eulerClient = \accessToken -> ET.client getFareAPI (Just $ "Bearer " <> accessToken) fareReq
      fareRes <- callCMRLV2API config eulerClient "getFare" getFareAPI
      logDebug $ "CMRL V2 Get Fares API Response : " <> show fareRes
      ticketCategoryMetadataConfig <- QFRFSTicketCategoryMetadataConfig.findByCategoryVehicleAndCity ADULT (becknVehicleCategoryToFrfsVehicleCategory integrationBPPConfig.vehicleCategory) integrationBPPConfig.merchantOperatingCityId
      fares <-
        case ((fareRes.result >>= (.discountedFare)) <|> (fareRes.result >>= (.fare))) of
          Just amount -> do
            let offeredPrice = amount
                originalPrice =
                  case (ticketCategoryMetadataConfig <&> (.domainCategoryValue)) of
                    Just domainCategoryValue ->
                      case domainCategoryValue of
                        FixedAmount discountAmount -> offeredPrice + discountAmount
                        Percentage discountPercentage -> HighPrecMoney $ offeredPrice.getHighPrecMoney / (1 - (toRational discountPercentage / 100))
                    Nothing -> offeredPrice
            return $
              [ FRFSUtils.FRFSFare
                  { categories =
                      [ FRFSUtils.FRFSTicketCategory
                          { category = ADULT,
                            price =
                              Price
                                { amountInt = round originalPrice,
                                  amount = originalPrice,
                                  currency = INR
                                },
                            offeredPrice =
                              Price
                                { amountInt = round amount,
                                  amount = amount,
                                  currency = INR
                                },
                            eligibility = True
                          }
                      ],
                    fareDetails = Nothing,
                    farePolicyId = Nothing,
                    vehicleServiceTier =
                      FRFSUtils.FRFSVehicleServiceTier
                        { serviceTierType = Spec.ORDINARY,
                          serviceTierProviderCode = "ORDINARY",
                          serviceTierShortName = "ORDINARY",
                          serviceTierDescription = "ORDINARY",
                          serviceTierLongName = "ORDINARY",
                          isAirConditioned = Just False
                        },
                    fareQuoteType = Nothing
                  }
              ]
          Nothing -> return []
      Hedis.setExp cacheKey fares 86400
      return fares
```

---

#### [NEW] GenerateTicket.hs

**Path**: `Backend/app/rider-platform/rider-app/Main/src/ExternalBPP/ExternalAPI/Metro/CMRL/v2/GenerateTicket.hs`

**Purpose**: Generate QR tickets with encryption

**Key Changes from v1**:
- Endpoint: `/api/qr/v1/tickets/generate` (v1 was `/CmrlThirdParty/generateqrticket`)
- Requires encryption headers: `X-ENC-ALGO`, `X-ENC-KEY-INDEX`
- Request body must be encrypted using AES_CBC_PKCS5

**Implementation**:
```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ExternalBPP.ExternalAPI.Metro.CMRL.v2.GenerateTicket where

import Data.Aeson
import qualified Data.Text as T
import qualified Data.UUID as UU
import Domain.Types.FRFSQuoteCategory
import Domain.Types.FRFSQuoteCategoryType
import Domain.Types.FRFSTicketBooking
import Domain.Types.IntegratedBPPConfig
import Domain.Types.Extra.IntegratedBPPConfig
import EulerHS.Types as ET hiding (Log)
import ExternalBPP.ExternalAPI.Metro.CMRL.v2.Auth
import ExternalBPP.ExternalAPI.Metro.CMRL.v2.Encryption
import ExternalBPP.ExternalAPI.Types
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.CacheFlow
import Kernel.Utils.Common
import Servant hiding (throwError)
import SharedLogic.FRFSUtils
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import Tools.Error

data GenerateTicketReq = GenerateTicketReq
  { request :: T.Text  -- Encrypted payload
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data GenerateTicketPayload = GenerateTicketPayload
  { operatorNameId :: Int,
    fromStationId :: T.Text,
    toStationId :: T.Text,
    ticketTypeId :: Int,
    merchantId :: T.Text,
    travelDatetime :: T.Text,
    fareTypeId :: Int,
    passengerCount :: Int,
    totalFare :: Int,
    merchantOrderId :: T.Text,
    paymentTransactionId :: T.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data TicketInfo = TicketInfo
  { ticketNumber :: T.Text,
    qrData :: T.Text,
    validFrom :: UTCTime,
    validUntil :: UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data GenerateTicketRes = GenerateTicketRes
  { statusCode :: Int,
    message :: T.Text,
    result :: [TicketInfo]
  }
  deriving (Generic, Show, ToJSON, FromJSON)

type GenerateTicketAPI =
  "api" :> "qr" :> "v1" :> "tickets" :> "generate"
    :> Header "Authorization" T.Text
    :> Header "X-ENC-ALGO" T.Text
    :> Header "X-ENC-KEY-INDEX" T.Text
    :> ReqBody '[JSON] GenerateTicketReq
    :> Post '[JSON] GenerateTicketRes

generateTicketAPI :: Proxy GenerateTicketAPI
generateTicketAPI = Proxy

createOrder :: (CoreMetrics m, MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r, HasShortDurationRetryCfg r c, HasRequestId r, MonadReader r m) => CMRLV2Config -> IntegratedBPPConfig -> FRFSTicketBooking -> [FRFSQuoteCategory] -> Maybe Text -> m ProviderOrder
createOrder config integratedBPPConfig booking quoteCategories _mRiderNumber = do
  orderId <- case booking.bppOrderId of
    Just oid -> return oid
    Nothing -> getBppOrderId booking
  paymentTxnId <- booking.paymentTxnId & fromMaybeM (InternalError $ "Payment Transaction Id Missing")
  fromStation <- OTPRest.getStationByGtfsIdAndStopCode booking.fromStationCode integratedBPPConfig >>= fromMaybeM (InternalError $ "Station not found for stationCode: " <> booking.fromStationCode)
  toStation <- OTPRest.getStationByGtfsIdAndStopCode booking.toStationCode integratedBPPConfig >>= fromMaybeM (InternalError $ "Station not found for stationCode: " <> booking.toStationCode)

  now <- getCurrentTime
  let travelDatetime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" now
      fareParameters = mkFareParameters (mkCategoryPriceItemFromQuoteCategories quoteCategories)
      singleAdultTicketPrice = find (\category -> category.categoryType == ADULT) fareParameters.priceItems <&> (.unitPrice.amountInt.getMoney)
      totalTicketQuantity = fareParameters.totalQuantity

      payload = GenerateTicketPayload
        { operatorNameId = config.operatorNameId,
          fromStationId = fromStation.code,
          toStationId = toStation.code,
          ticketTypeId = config.ticketTypeId,  -- From config, not hardcoded
          merchantId = config.merchantId,
          travelDatetime = travelDatetime,
          fareTypeId = config.fareTypeId,  -- From config, not hardcoded
          passengerCount = totalTicketQuantity,
          totalFare = fromMaybe 0 singleAdultTicketPrice,
          merchantOrderId = orderId,
          paymentTransactionId = paymentTxnId
        }

  encryptedPayload <- encryptPayload config (encode payload)

  let eulerClient = \accessToken ->
        ET.client generateTicketAPI
          (Just $ "Bearer " <> accessToken)
          (Just "AES_CBC_PKCS5")
          (Just config.encKeyIndex)
          (GenerateTicketReq encryptedPayload)

  ticketRes <- callCMRLV2API config eulerClient "generateTicket" generateTicketAPI

  tickets <- ticketRes.result `forM` \TicketInfo {..} -> do
    return $
      ProviderTicket
        { ticketNumber = ticketNumber,
          vehicleNumber = Nothing,
          qrData = qrData,
          qrStatus = "UNCLAIMED",
          qrValidity = validUntil,
          description = Nothing,
          qrRefreshAt = Nothing,
          commencingHours = Nothing
        }

  return ProviderOrder {..}

getBppOrderId :: (MonadFlow m) => FRFSTicketBooking -> m Text
getBppOrderId booking = do
  bookingUUID <- UU.fromText booking.id.getId & fromMaybeM (InternalError "Booking Id not being able to parse into UUID")
  let orderId = T.pack $ "CUM" ++ show ((\(a, b, c, d) -> a + b + c + d) (UU.toWords bookingUUID))
  return orderId
```

---

#### [NEW] TicketStatus.hs

**Path**: `Backend/app/rider-platform/rider-app/Main/src/ExternalBPP/ExternalAPI/Metro/CMRL/v2/TicketStatus.hs`

**Purpose**: Check ticket status

**Key Changes from v1**:
- Endpoint: `/api/qr/v1/tickets/{merchantOrderId}/status` (v1 was `/CmrlThirdParty/ticketStatus`)
- Path parameter instead of query parameter
- Requires `operatorNameId` and `merchantId` as query params

**Implementation**:
```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ExternalBPP.ExternalAPI.Metro.CMRL.v2.TicketStatus where

import Data.Aeson
import qualified Data.Text as T
import Domain.Types.FRFSTicketBooking
import qualified Domain.Types.FRFSTicketStatus as Ticket
import Domain.Types.Extra.IntegratedBPPConfig
import EulerHS.Types as ET
import ExternalBPP.ExternalAPI.Metro.CMRL.v2.Auth
import ExternalBPP.ExternalAPI.Types
import qualified Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Types.App
import Kernel.Utils.Common
import Servant
import qualified Storage.Queries.FRFSTicket as QFRFSTicket

data TicketStatusResult = TicketStatusResult
  { ticketNumber :: T.Text,
    ticketStatus :: T.Text,
    validFrom :: UTCTime,
    validUntil :: UTCTime,
    usedAt :: Maybe UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data TicketStatusRes = TicketStatusRes
  { statusCode :: Int,
    message :: T.Text,
    result :: TicketStatusResult
  }
  deriving (Generic, Show, ToJSON, FromJSON)

type TicketStatusAPI =
  "api" :> "qr" :> "v1" :> "tickets" :> Capture "merchantOrderId" T.Text :> "status"
    :> Header "Authorization" T.Text
    :> QueryParam' '[Required, Strict] "operatorNameId" Int
    :> QueryParam' '[Required, Strict] "merchantId" T.Text
    :> Get '[JSON] TicketStatusRes

ticketStatusAPI :: Proxy TicketStatusAPI
ticketStatusAPI = Proxy

getTicketStatus :: (MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r) => CMRLV2Config -> FRFSTicketBooking -> m [ProviderTicket]
getTicketStatus config booking = do
  bppOrderId <- case booking.bppOrderId of
    Just oid -> return oid
    Nothing -> throwError $ InternalError "BPP Order ID not found"

  let eulerClient = \accessToken ->
        ET.client ticketStatusAPI bppOrderId (Just $ "Bearer " <> accessToken) config.operatorNameId config.merchantId

  ticketStatusRes <- callCMRLV2API config eulerClient "getTicketStatus" ticketStatusAPI

  tickets <- B.runInReplica $ QFRFSTicket.findAllByTicketBookingId booking.id

  return $ map (\ticket ->
    ProviderTicket
      { ticketNumber = ticket.ticketNumber,
        vehicleNumber = Nothing,
        qrData = ticket.qrData,
        qrStatus = mkTicketStatus ticketStatusRes.result.ticketStatus,
        qrValidity = ticketStatusRes.result.validUntil,
        description = ticket.description,
        qrRefreshAt = ticket.qrRefreshAt,
        commencingHours = ticket.commencingHours
      }
  ) tickets
  where
    mkTicketStatus status =
      case status of
        "USED" -> "CLAIMED"
        "EXPIRED" -> "EXPIRED"
        _ -> "UNCLAIMED"
```

---

#### [NEW] Error.hs

**Path**: `Backend/app/rider-platform/rider-app/Main/src/ExternalBPP/ExternalAPI/Metro/CMRL/v2/Error.hs`

**Purpose**: Error handling for v2 APIs

**Implementation**:
```haskell
{-# LANGUAGE TemplateHaskell #-}

module ExternalBPP.ExternalAPI.Metro.CMRL.v2.Error where

import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Error.BaseError.HTTPError

data CMRLV2Error
  = CMRLV2BadRequest Text
  | CMRLV2Unauthorized Text
  | CMRLV2InternalError Text
  | CMRLV2ServiceUnavailable Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''CMRLV2Error

instance IsBaseError CMRLV2Error where
  toMessage = \case
    CMRLV2BadRequest msg -> Just msg
    CMRLV2Unauthorized msg -> Just msg
    CMRLV2InternalError msg -> Just msg
    CMRLV2ServiceUnavailable msg -> Just msg

instance IsHTTPError CMRLV2Error where
  toErrorCode = \case
    CMRLV2BadRequest _ -> "BAD_REQUEST"
    CMRLV2Unauthorized _ -> "UNAUTHORIZED"
    CMRLV2InternalError _ -> "INTERNAL_ERROR"
    CMRLV2ServiceUnavailable _ -> "SERVICE_UNAVAILABLE"

  toHttpCode = \case
    CMRLV2BadRequest _ -> E400
    CMRLV2Unauthorized _ -> E401
    CMRLV2InternalError _ -> E500
    CMRLV2ServiceUnavailable _ -> E503

instance IsAPIError CMRLV2Error
```

---

#### [NEW] Encryption.hs

**Path**: `Backend/app/rider-platform/rider-app/Main/src/ExternalBPP/ExternalAPI/Metro/CMRL/v2/Encryption.hs`

**Purpose**: Handle AES encryption/decryption for ticket generation

**Implementation**:
```haskell
module ExternalBPP.ExternalAPI.Metro.CMRL.v2.Encryption where

import qualified Crypto.Cipher.AES as AES
import qualified Crypto.Cipher.Types as CT
import qualified Crypto.Error as CE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Domain.Types.Extra.IntegratedBPPConfig
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Utils.Common

-- Encrypt payload using AES-256-CBC with PKCS5 padding
encryptPayload :: (MonadFlow m, EncFlow m r, MonadReader r m) => CMRLV2Config -> BL.ByteString -> m Text
encryptPayload config payload = do
  encKey <- decrypt config.encryptionKey
  let keyBytes = TE.encodeUtf8 encKey
      -- Use first 16 bytes as IV (or generate random IV)
      iv = BS.take 16 keyBytes
      key = BS.take 32 keyBytes  -- AES-256 requires 32 bytes
      payloadBytes = BL.toStrict payload
      paddedPayload = pkcs5Pad payloadBytes

  case (CE.eitherCryptoError $ CT.cipherInit key :: Either CE.CryptoError AES.AES256,
        CE.eitherCryptoError $ CT.makeIV iv) of
    (Right cipher, Just initIV) -> do
      let encrypted = CT.cbcEncrypt cipher initIV paddedPayload
          encoded = B64.encode encrypted
      return $ TE.decodeUtf8 encoded
    _ -> throwError $ InternalError "Failed to initialize AES cipher"

-- Decrypt response
decryptResponse :: (MonadFlow m, EncFlow m r, MonadReader r m) => CMRLV2Config -> Text -> m BL.ByteString
decryptResponse config encryptedText = do
  encKey <- decrypt config.encryptionKey
  let keyBytes = TE.encodeUtf8 encKey
      iv = BS.take 16 keyBytes
      key = BS.take 32 keyBytes
      encryptedBytes = B64.decodeLenient $ TE.encodeUtf8 encryptedText

  case (CE.eitherCryptoError $ CT.cipherInit key :: Either CE.CryptoError AES.AES256,
        CE.eitherCryptoError $ CT.makeIV iv) of
    (Right cipher, Just initIV) -> do
      let decrypted = CT.cbcDecrypt cipher initIV encryptedBytes
          unpadded = pkcs5Unpad decrypted
      return $ BL.fromStrict unpadded
    _ -> throwError $ InternalError "Failed to initialize AES cipher for decryption"

-- PKCS5 padding
pkcs5Pad :: BS.ByteString -> BS.ByteString
pkcs5Pad bs =
  let blockSize = 16
      len = BS.length bs
      padLen = blockSize - (len `mod` blockSize)
      padding = BS.replicate padLen (fromIntegral padLen)
  in bs <> padding

-- PKCS5 unpadding
pkcs5Unpad :: BS.ByteString -> BS.ByteString
pkcs5Unpad bs =
  if BS.null bs
    then bs
    else
      let padLen = fromIntegral $ BS.last bs
      in BS.take (BS.length bs - padLen) bs
```

---

### Component 2: Configuration Updates

#### [MODIFY] IntegratedBPPConfig.hs

**Path**: `Backend/app/rider-platform/rider-app/Main/src/Domain/Types/Extra/IntegratedBPPConfig.hs`

Add new configuration type for CMRL v2 after the existing `CMRLConfig`:

```haskell
data CMRLV2Config = CMRLV2Config
  { networkHostUrl :: BaseUrl,
    username :: Text,
    password :: EncryptedField 'AsEncrypted Text,
    operatorNameId :: Int,
    merchantId :: Text,
    encKeyIndex :: Text,
    encryptionKey :: EncryptedField 'AsEncrypted Text,
    ticketTypeId :: Int,  -- Ticket type: 1=SJT, 102=RJT, 104=GROUP, 105=FAMILY, 103=SVP
    fareTypeId :: Int     -- Fare type: 1001=General, 1002=Flat, etc.
  }
  deriving stock (Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Show CMRLV2Config where
  show _ = "CMRLV2Config"
```

You'll also need to update the `ProviderConfig` type in the read-only generated file to include `CMRLV2`.

---

### Component 3: CallAPI Integration

#### [MODIFY] CallAPI.hs

**Path**: `Backend/app/rider-platform/rider-app/Main/src/ExternalBPP/ExternalAPI/CallAPI.hs`

**1. Add imports** (after line 33):
```haskell
import qualified ExternalBPP.ExternalAPI.Metro.CMRL.V2.Auth as CMRLV2Auth
import qualified ExternalBPP.ExternalAPI.Metro.CMRL.V2.GetFare as CMRLV2GetFare
import qualified ExternalBPP.ExternalAPI.Metro.CMRL.V2.StationList as CMRLV2StationList
import qualified ExternalBPP.ExternalAPI.Metro.CMRL.V2.BusinessHour as CMRLV2BusinessHour
import qualified ExternalBPP.ExternalAPI.Metro.CMRL.V2.GenerateTicket as CMRLV2GenerateTicket
import qualified ExternalBPP.ExternalAPI.Metro.CMRL.V2.TicketStatus as CMRLV2TicketStatus
```

**2. Update `getProviderName`** (around line 56):
```haskell
getProviderName integrationBPPConfig =
  case (integrationBPPConfig.providerName, integrationBPPConfig.providerConfig) of
    (Just name, _) -> name
    (_, CMRL _) -> "Chennai Metro Rail Limited"
    (_, CMRLV2 _) -> "Chennai Metro Rail Limited v2"
    (_, EBIX _) -> "Kolkata Buses"
    (_, DIRECT _) -> "Direct Multimodal Services"
    (_, ONDC _) -> "ONDC Services"
    (_, CRIS _) -> "CRIS Subway"
```

**3. Update `getFares`** (add case after CMRL case around line 85):\n```haskell
    CMRLV2 config' -> do
      now <- getCurrentTime
      let travelDatetime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" now
      fares <-
        CMRLV2GetFare.getFare integrationBPPConfig config' $
          CMRLV2GetFare.GetFareReq
            { operatorNameId = config'.operatorNameId,
              fromStationId = startStopCode,
              toStationId = endStopCode,
              ticketTypeId = config'.ticketTypeId,  -- From config
              merchantId = config'.merchantId,
              travelDatetime = travelDatetime,
              fareTypeId = config'.fareTypeId  -- From config
            }
      return (isFareMandatory, fares)
```

**4. Update `createOrder`** (add case around line 173):
```haskell
      CMRLV2 config' -> CMRLV2GenerateTicket.createOrder config' integrationBPPConfig booking quoteCategories mRiderNumber
```

**5. Update `getBppOrderId`** (add case around line 184):
```haskell
    CMRLV2 _ -> Just <$> CMRLV2GenerateTicket.getBppOrderId booking
```

**6. Update `getTicketStatus`** (add case around line 193):
```haskell
    CMRLV2 config' -> CMRLV2TicketStatus.getTicketStatus config' booking
```

**7. Update `getBusinessHour`** (add case around line 220):
```haskell
    CMRLV2 config' -> CMRLV2BusinessHour.getBusinessHour config'
```

**8. Update `getStationList`** (add case around line 244):
```haskell
    CMRLV2 config' -> CMRLV2StationList.getStationList config'
```

**9. Add new function** (after `getStationList`):
```haskell
getTicketTypes :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r, HasRequestId r, MonadReader r m) => IntegratedBPPConfig -> m [CMRLV2TicketType.TicketType]
getTicketTypes integrationBPPConfig = do
  case integrationBPPConfig.providerConfig of
    CMRLV2 config' -> CMRLV2TicketType.getTicketTypes config'
    _ -> throwError $ InternalError "Unimplemented!"
```

---

## Verification Plan

### Automated Tests

1. **Unit Tests for Each v2 Module**
2. **Integration Tests** - Complete flow testing
3. **Encryption Tests** - Verify AES-256-CBC encryption/decryption

### Manual Verification

1. **Configuration Setup** - Add CMRLV2 configuration to database
2. **API Testing** - Test each endpoint independently
3. **End-to-End Flow** - Complete user journey
4. **Backward Compatibility** - Ensure v1 continues to work

---

## Migration Strategy

1. **Phase 1**: Deploy v2 APIs alongside v1 (both active)
2. **Phase 2**: Test v2 in staging environment
3. **Phase 3**: Gradual rollout to production (feature flag based)
4. **Phase 4**: Monitor metrics and error rates
5. **Phase 5**: Deprecate v1 after successful v2 adoption

## Dependencies

- AES encryption library (`cryptonite` package)
- Base64 encoding utilities
- Time formatting utilities

## Estimated Effort

- **Configuration**: 2 hours
- **v2 API Modules**: 8-10 hours
- **CallAPI Integration**: 3-4 hours
- **Testing**: 4-5 hours
- **Total**: ~20 hours
