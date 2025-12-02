{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ExternalBPP.ExternalAPI.Metro.CMRL.UpdateQrReceivedStatus where

import Data.Aeson
import qualified Data.Text as T
import Domain.Types.IntegratedBPPConfig
import EulerHS.Types as ET
import ExternalBPP.ExternalAPI.Metro.CMRL.Auth
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.App
import Kernel.Utils.Common
import Servant

data UpdateQrReceivedStatusReq = UpdateQrReceivedStatusReq
  { txnRefNo :: T.Text,
    appType :: T.Text,
    isFailure :: Bool,
    failureReason :: T.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data UpdateQrReceivedStatusResult = UpdateQrReceivedStatusResult
  { txnRefNo :: T.Text,
    isUpdated :: Bool,
    message :: T.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data UpdateQrReceivedStatusAPIRes = UpdateQrReceivedStatusAPIRes
  { statusCode :: Int,
    message :: T.Text,
    result :: UpdateQrReceivedStatusResult
  }
  deriving (Generic, Show, ToJSON, FromJSON)

type UpdateQrReceivedStatusAPI =
  "CmrlThirdParty" :> "updateQrReceivedStatus"
    :> Header "Authorization" T.Text
    :> ReqBody '[JSON] UpdateQrReceivedStatusReq
    :> Post '[JSON] UpdateQrReceivedStatusAPIRes

updateQrReceivedStatusAPI :: Proxy UpdateQrReceivedStatusAPI
updateQrReceivedStatusAPI = Proxy

updateQrReceivedStatus ::
  (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r, HasRequestId r, MonadReader r m) =>
  CMRLConfig ->
  UpdateQrReceivedStatusReq ->
  m UpdateQrReceivedStatusResult
updateQrReceivedStatus config req = do
  let eulerClient = \accessToken -> ET.client updateQrReceivedStatusAPI (Just $ "Bearer " <> accessToken) req
  response <- callCMRLAPI config eulerClient "updateQrReceivedStatus" updateQrReceivedStatusAPI
  return response.result
