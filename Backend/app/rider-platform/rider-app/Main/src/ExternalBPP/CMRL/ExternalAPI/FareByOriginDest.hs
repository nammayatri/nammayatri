{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ExternalBPP.CMRL.ExternalAPI.FareByOriginDest where

import Data.Aeson
import qualified Data.Text as T
import Domain.Types.IntegratedBPPConfig
import EulerHS.Types as ET
import GHC.Generics (Generic)
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Servant
import Tools.Error

data FareByOriginDestReq = FareByOriginDestReq
  { origin :: T.Text,
    destination :: T.Text,
    ticketType :: T.Text
  }
  deriving (Generic, Show)

instance ToJSON FareByOriginDestReq where
  toJSON = genericToJSON defaultOptions

data FareByOriginDestResInner = FareByOriginDestResInner
  { result :: Int
  }
  deriving (Generic, Show)

instance FromJSON FareByOriginDestResInner where
  parseJSON = genericParseJSON defaultOptions

data FareByOriginDestAPIRes = FareByOriginDestAPIRes
  { statusCode :: Int,
    message :: T.Text,
    result :: FareByOriginDestResInner
  }
  deriving (Generic, Show)

instance FromJSON FareByOriginDestAPIRes where
  parseJSON = genericParseJSON defaultOptions

type FareByOriginDestAPI =
  "cumta" :> "farebyod"
    :> Header "Authorization" T.Text
    :> ReqBody '[JSON] FareByOriginDestReq
    :> Get '[JSON] FareByOriginDestAPIRes

fareByOriginDestAPI :: Proxy FareByOriginDestAPI
fareByOriginDestAPI = Proxy

getFareByOriginDest :: (CoreMetrics m, MonadFlow m) => T.Text -> T.Text -> FareByOriginDestReq -> m Int
getFareByOriginDest host accessToken fareReq = do
  fareByODRes <-
    callAPI host (ET.client fareByOriginDestAPI (Just $ "Bearer " <> accessToken) fareReq) "getFareByOriginDest" fareByOriginDestAPI
      >>= fromEitherM (ExternalAPICallError (Just "CMRL_FARE_BY_ORIGIN_DEST_API") host)
  return fareByODRes.result.result
