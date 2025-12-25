module ExternalBPP.ExternalAPI.Subway.CRIS.SDKData where

import API.Types.UI.CRIS as CRIS
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Domain.Types.Extra.IntegratedBPPConfig (CRISConfig)
import Domain.Types.Person
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import ExternalBPP.ExternalAPI.Subway.CRIS.Auth (callCRISAPI)
import ExternalBPP.ExternalAPI.Subway.CRIS.Encryption (encryptPayload)
import Kernel.External.Encryption
import Kernel.Prelude hiding (app)
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.App
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant.API
import qualified Storage.Queries.Person as QPerson

data EncryptedRequest = EncryptedRequest
  { app :: Text,
    data_ :: Text
  }
  deriving (Generic, Show)

instance ToJSON EncryptedRequest where
  toJSON EncryptedRequest {..} =
    object ["app" .= app, "data" .= data_]

type SDKDataAPI =
  "t" :> "uts.cris.in" :> "VBCU" :> "1" :> "get-sdk-data"
    :> Header "Authorization" Text
    :> Header "Content-Type" Text
    :> Header "appCode" Text
    :> ReqBody '[JSON] EncryptedRequest
    :> Post '[JSON] CRIS.GetSDKDataResponse

mkGetSDKDataReq :: (CoreMetrics m, MonadFlow m, EsqDBFlow m r, EncFlow m r, CacheFlow m r) => Id Person -> m (Maybe CRIS.GetSDKDataRequest)
mkGetSDKDataReq personId = do
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  mbMobileNumber <- mapM decrypt person.mobileNumber
  mbImeiNumber <- mapM decrypt person.imeiNumber
  case (mbMobileNumber, mbImeiNumber) of
    (Just mobile, Just imei) -> return $ Just $ CRIS.GetSDKDataRequest {mobileNo = mobile, deviceID = imei}
    _ -> return Nothing

getSDKData ::
  ( CoreMetrics m,
    MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r
  ) =>
  CRISConfig ->
  CRIS.GetSDKDataRequest ->
  m CRIS.GetSDKDataResponse
getSDKData config request = do
  let mobileNo = request.mobileNo
  let deviceID = request.deviceID
  let sdkDataRequest =
        object
          [ "mobileNo" .= mobileNo,
            "agentAccountID" .= (show config.tpAccountId :: Text),
            "deviceID" .= deviceID
          ]
  let jsonStr = decodeUtf8 $ LBS.toStrict $ encode sdkDataRequest
  logDebug $ "getSDKData Req: " <> jsonStr
  encryptionKey <- decrypt config.encryptionKey
  payload <- encryptPayload jsonStr encryptionKey
  let encReq =
        EncryptedRequest
          { app = config.appCode,
            data_ = payload
          }
  sdkData <- callCRISAPI config sdkDataAPI (eulerClientFn encReq) "getSDKData"
  logDebug $ "getSDKData Res: " <> show sdkData
  return sdkData
  where
    eulerClientFn payload token =
      let client = ET.client sdkDataAPI
       in client (Just $ "Bearer " <> token) (Just "application/json") (Just "CUMTA") payload

sdkDataAPI :: Proxy SDKDataAPI
sdkDataAPI = Proxy
