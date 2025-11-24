module ExternalBPP.ExternalAPI.Subway.CRIS.ReconTicketEnquiry where

import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Domain.Types.Extra.IntegratedBPPConfig (CRISConfig)
import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import EulerHS.Prelude hiding (concatMap, find, null, readMaybe, whenJust)
import qualified EulerHS.Types as ET
import ExternalBPP.ExternalAPI.Subway.CRIS.Auth (callCRISAPI)
import ExternalBPP.ExternalAPI.Subway.CRIS.Encryption (decryptResponseData, encryptPayload)
import ExternalBPP.ExternalAPI.Subway.CRIS.Types
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.App
import Kernel.Types.Error
import Kernel.Utils.Common
import Servant.API
import qualified Storage.Queries.Person as QPerson

-- API type for recon ticket enquiry
type ReconTicketEnquiryAPI =
  "t" :> "uts.cris.in" :> "VCU" :> "1" :> "recon" :> "ticketEnquiry"
    :> Header "Authorization" Text
    :> Header "Content-Type" Text
    :> Header "appCode" Text
    :> ReqBody '[PlainText] Text
    :> Post '[JSON] EncryptedResponse

-- Request type for recon ticket enquiry
data ReconTicketEnquiryRequest = ReconTicketEnquiryRequest
  { othAppTxnId :: Text,
    fareAmount :: HighPrecMoney,
    mobileNo :: Text,
    appCode :: Text
  }
  deriving (Generic, ToJSON)

-- Response type for recon ticket enquiry
data ReconTicketEnquiryResponse = ReconTicketEnquiryResponse
  { responseCode :: Text,
    responseData :: Text
  }
  deriving (Generic, FromJSON)

-- Transaction details type
data TxnDetails = TxnDetails
  { appTxnId :: Text,
    othAppTxnId :: Text,
    fare :: HighPrecMoney,
    txnTime :: Text,
    txnStatus :: Int,
    txnStatusDesc :: Text
  }
  deriving (Generic, FromJSON)

-- Decrypted response type
data ReconTicketEnquiryDecryptedResponse = ReconTicketEnquiryDecryptedResponse
  { txnDetailsList :: [TxnDetails]
  }
  deriving (Generic, FromJSON)

-- Main function
getReconTicketEnquiry ::
  ( CoreMetrics m,
    MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r
  ) =>
  CRISConfig ->
  DFRFSTicketBooking.FRFSTicketBooking ->
  m ReconTicketEnquiryDecryptedResponse
getReconTicketEnquiry config frfsTicketBooking = do
  bppOrderId <- frfsTicketBooking.bppOrderId & fromMaybeM (InvalidRequest "BppOrderId is missing")
  let fareAmount = frfsTicketBooking.totalPrice.amount
  person <- QPerson.findById frfsTicketBooking.riderId >>= fromMaybeM (PersonNotFound frfsTicketBooking.riderId.getId)
  mobileNumber <- person.mobileNumber & fromMaybeM (InvalidRequest "mobile no. not found")
  decryptedMobileNumber <- decrypt mobileNumber

  let reconRequest =
        ReconTicketEnquiryRequest
          { othAppTxnId = bppOrderId,
            fareAmount = fareAmount,
            mobileNo = decryptedMobileNumber,
            appCode = config.appCode
          }

  let jsonStr = decodeUtf8 $ LBS.toStrict $ encode reconRequest

  logInfo $ "getReconTicketEnquiry Req: " <> jsonStr

  encryptionKey <- decrypt config.encryptionKey
  decryptionKey <- decrypt config.decryptionKey
  payload <- encryptPayload jsonStr encryptionKey

  encryptedResponse <- callCRISAPI config reconTicketEnquiryAPI (eulerClientFn payload) "getReconTicketEnquiry"

  logInfo $ "getReconTicketEnquiry Resp: " <> show encryptedResponse

  -- Decrypt the response
  decryptedResponse :: ReconTicketEnquiryDecryptedResponse <- case eitherDecode (encode encryptedResponse) :: Either String ReconTicketEnquiryResponse of
    Left err -> throwError (InternalError $ "Failed to parse encrypted getReconTicketEnquiry Resp: " <> T.pack (show err))
    Right encResp -> do
      logInfo $ "getReconTicketEnquiry Resp Code: " <> encResp.responseCode
      if encResp.responseCode == "0"
        then do
          case decryptResponseData encResp.responseData decryptionKey of
            Left err -> throwError (InternalError $ "Failed to decrypt getReconTicketEnquiry Resp: " <> T.pack err)
            Right decryptedJson -> do
              logInfo $ "getReconTicketEnquiry Decrypted Resp: " <> decryptedJson
              case eitherDecode (LBS.fromStrict $ TE.encodeUtf8 decryptedJson) of
                Left err -> throwError (InternalError $ "Failed to decode getReconTicketEnquiry Resp: " <> T.pack (show err))
                Right reconResponse -> pure reconResponse
        else throwError (InternalError $ "Non-zero response code in getReconTicketEnquiry Resp: " <> encResp.responseCode <> " " <> encResp.responseData)

  return decryptedResponse
  where
    eulerClientFn payload token =
      let client = ET.client reconTicketEnquiryAPI
       in client (Just $ "Bearer " <> token) (Just "application/json") (Just "CUMTA") payload

reconTicketEnquiryAPI :: Proxy ReconTicketEnquiryAPI
reconTicketEnquiryAPI = Proxy
