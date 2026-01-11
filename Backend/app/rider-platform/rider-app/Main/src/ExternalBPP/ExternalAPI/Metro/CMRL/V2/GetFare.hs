{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ExternalBPP.ExternalAPI.Metro.CMRL.V2.GetFare where

import qualified BecknV2.FRFS.Enums as Spec
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Domain.Types.FRFSQuoteCategoryType
import Domain.Types.IntegratedBPPConfig
import EulerHS.Types as ET
import ExternalBPP.ExternalAPI.Metro.CMRL.V2.Auth
import ExternalBPP.ExternalAPI.Metro.CMRL.V2.Encryption
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.App
import Kernel.Utils.Common
import Servant hiding (throwError)
import qualified SharedLogic.FRFSUtils as FRFSUtils
import Tools.Error

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

data EncryptedReq = EncryptedReq
  { request :: T.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data EncryptedRes = EncryptedRes
  { response :: T.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data GetFareItem = GetFareItem
  { fromStationId :: T.Text,
    toStationId :: T.Text,
    fareBeforeDiscount :: Double,
    discountAmount :: Double,
    fareAfterDiscount :: Double,
    cgst :: Double,
    sgst :: Double,
    finalFare :: Double,
    fareValidTime :: T.Text,
    fareQuotIdforOneTicket :: T.Text,
    returnCode :: T.Text,
    returnMsg :: T.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

type GetFareRes = [GetFareItem]

type GetFareAPI =
  "api" :> "qr" :> "v1" :> "fare" :> "getfare"
    :> Header "Authorization" T.Text
    :> Header "X-ENC-ALGO" T.Text
    :> Header "X-ENC-KEY-INDEX" T.Text
    :> ReqBody '[JSON] EncryptedReq
    :> Post '[JSON] EncryptedRes

getFareAPI :: Proxy GetFareAPI
getFareAPI = Proxy

getFare :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r, EsqDBFlow m r, HasRequestId r, MonadReader r m) => IntegratedBPPConfig -> CMRLV2Config -> T.Text -> GetFareReq -> m [FRFSUtils.FRFSFare]
getFare _ config _riderId fareReq = do
  logInfo $ "[CMRLV2:GetFare] Getting fare from: " <> fareReq.fromStationId <> " to: " <> fareReq.toStationId
  logDebug $ "[CMRLV2:GetFare] Request params - operatorNameId: " <> show fareReq.operatorNameId <> ", ticketTypeId: " <> show fareReq.ticketTypeId <> ", fareTypeId: " <> show fareReq.fareTypeId

  -- Get encryption key
  encKey <- decrypt config.encryptionKey

  -- Prepare payload and encrypt
  logDebug $ "[CMRLV2:GetFare] Payload JSON (before encryption): " <> T.pack (show fareReq)
  let payloadText = TE.decodeUtf8 $ BL.toStrict $ encode fareReq
  logDebug $ "[CMRLV2:GetFare] Payload Text: " <> payloadText
  encryptedPayload <- encryptPayload payloadText encKey
  logDebug $ "[CMRLV2:GetFare] Encrypted Payload: " <> encryptedPayload

  let eulerClient = \accessToken ->
        ET.client
          getFareAPI
          (Just $ "Bearer " <> accessToken)
          (Just "AES_CBC_PKCS5")
          (Just $ T.pack $ show config.encKeyIndex)
          (EncryptedReq encryptedPayload)

  -- Call API
  encryptedResponse <- callCMRLV2API config eulerClient "getFare" getFareAPI
  logDebug $ "[CMRLV2:GetFare] Encrypted Response: " <> encryptedResponse.response

  -- Decrypt response
  decryptedResponseText <- case decryptPayload encryptedResponse.response encKey of
    Left err -> do
      logError $ "[CMRLV2:GetFare] Decryption failed: " <> T.pack err
      throwError $ InternalError $ "GetFare decryption failed: " <> T.pack err
    Right txt -> return txt
  logDebug $ "[CMRLV2:GetFare] Decrypted Response: " <> decryptedResponseText

  -- Parse response
  fareRes <- case eitherDecode (BL.fromStrict $ TE.encodeUtf8 decryptedResponseText) :: Either String GetFareRes of
    Left err -> do
      logError $ "[CMRLV2:GetFare] Failed to decode fare response: " <> T.pack err
      throwError $ InternalError $ "Failed to decode fare response: " <> T.pack err
    Right res -> do
      logDebug $ "[CMRLV2:GetFare] Parsed response successfully, items count: " <> show (length res)
      return res

  logDebug $ "[CMRLV2:GetFare] API Response: " <> T.pack (show fareRes)
  case fareRes of
    (fareItem : _) -> do
      if fareItem.returnCode == "0"
        then do
          let originalPrice = HighPrecMoney $ toRational fareItem.fareBeforeDiscount
              offeredPrice = HighPrecMoney $ toRational fareItem.finalFare
          logDebug $ "[CMRLV2:GetFare] Using API values - fareBeforeDiscount: " <> T.pack (show fareItem.fareBeforeDiscount) <> ", finalFare: " <> T.pack (show fareItem.finalFare) <> ", discountAmount: " <> T.pack (show fareItem.discountAmount)
          return
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
                              { amountInt = round offeredPrice,
                                amount = offeredPrice,
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
        else do
          logWarning $ "[CMRLV2:GetFare] API returned error: returnCode=" <> fareItem.returnCode <> ", returnMsg=" <> fareItem.returnMsg
          return []
    [] -> do
      logDebug "[CMRLV2:GetFare] No fare found in response (empty array)"
      return []
