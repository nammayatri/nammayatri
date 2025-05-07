{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module ExternalBPP.ExternalAPI.Subway.CRIS.Uts where

import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Domain.Types.IntegratedBPPConfig
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import ExternalBPP.ExternalAPI.Subway.CRIS.Auth (callCRISAPI)
import ExternalBPP.ExternalAPI.Subway.CRIS.Encryption (decryptResponseData)
import Kernel.External.Encryption
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.App
import Kernel.Types.Error
import Kernel.Utils.Common
import Servant.API

-- Request and Response types
data UtsRequest = UtsRequest
  { app :: Text
  }
  deriving (Show, Generic)

instance ToJSON UtsRequest

data UtsResponse = UtsResponse
  { respCode :: Int,
    respMessage :: Text,
    utsData :: Text
  }
  deriving (Show, Generic)

instance FromJSON UtsResponse

instance ToJSON UtsResponse

data DecodedUtsData = DecodedUtsData
  { respCode :: Int,
    respMessage :: Text,
    url :: Text,
    fare :: Text,
    tkt :: Text
  }
  deriving (Show, Generic)

instance FromJSON DecodedUtsData

-- API Type
type UtsAPI =
  "t" :> "uts.cris.in" :> "VBCU" :> "1" :> "get-uts-data"
    :> Header "Authorization" Text
    :> Header "Content-Type" Text
    :> ReqBody '[JSON] UtsRequest
    :> Post '[JSON] UtsResponse

utsAPI :: Proxy UtsAPI
utsAPI = Proxy

getUtsData ::
  ( CoreMetrics m,
    MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r
  ) =>
  CRISConfig ->
  m DecodedUtsData
getUtsData config = do
  let req = UtsRequest {app = config.appCode}
  logInfo $ "UTS request: " <> show req
  resp <- callCRISAPI config utsAPI (eulerClientFn req) "getUtsData"
  logInfo $ "UTS response: " <> show resp

  utsKey <- decrypt config.utsDataKey

  -- Decrypt utsData
  if resp.respCode == 0
    then do
      case decryptResponseData resp.utsData utsKey of
        Left err -> throwError $ InternalError $ "Failed to decrypt key data: " <> T.pack err
        Right decryptedJson -> do
          logInfo $ "Decrypted uts data: " <> decryptedJson
          case eitherDecode (LBS.fromStrict $ TE.encodeUtf8 decryptedJson) :: Either String DecodedUtsData of
            Left err -> throwError $ InternalError $ "Failed to parse uts data: " <> T.pack err
            Right utsData -> pure utsData
    else throwError $ InternalError $ "Fetching uts data failed with code: " <> (show $ resp.respCode)
  where
    eulerClientFn req token =
      let client = ET.client utsAPI
       in client (Just $ "Bearer " <> token) (Just "application/json") req
