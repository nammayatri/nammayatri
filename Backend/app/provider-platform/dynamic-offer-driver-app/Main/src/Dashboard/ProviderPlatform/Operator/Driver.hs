{-# OPTIONS_GHC -Wno-orphans #-}

module Dashboard.ProviderPlatform.Operator.Driver where

import qualified API.Types.ProviderPlatform.Operator.Endpoints.Driver as CommonDriver
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Kernel.Prelude
import Servant

instance FromHttpApiData CommonDriver.EntityType where
  parseUrlPiece txt = case txt of
    "DRIVER" -> Right CommonDriver.DRIVER
    "VEHICLE" -> Right CommonDriver.VEHICLE
    "FLEET_OWNER" -> Right CommonDriver.FLEET_OWNER
    _ -> Left "Invalid EntityType"

instance ToHttpApiData CommonDriver.EntityType where
  toUrlPiece CommonDriver.DRIVER = "DRIVER"
  toUrlPiece CommonDriver.VEHICLE = "VEHICLE"
  toUrlPiece CommonDriver.FLEET_OWNER = "FLEET_OWNER"

instance FromHttpApiData CommonDriver.ReviewRequestType where
  parseUrlPiece txt = case txt of
    "BOT_REVIEW" -> Right CommonDriver.BOT_REVIEW
    _ -> Left "Invalid ReviewRequestType"

instance ToHttpApiData CommonDriver.ReviewRequestType where
  toUrlPiece CommonDriver.BOT_REVIEW = "BOT_REVIEW"

instance FromHttpApiData CommonDriver.ReviewRequestStatus where
  parseUrlPiece txt = case txt of
    "RR_IN_PROGRESS" -> Right CommonDriver.RR_IN_PROGRESS
    "RR_COMPLETED" -> Right CommonDriver.RR_COMPLETED
    "RR_REJECTED" -> Right CommonDriver.RR_REJECTED
    _ -> case Aeson.eitherDecode (BSL.fromStrict $ TE.encodeUtf8 txt) of
      Right val -> Right val
      Left err -> Left $ T.pack err

instance ToHttpApiData CommonDriver.ReviewRequestStatus where
  toUrlPiece CommonDriver.RR_IN_PROGRESS = "RR_IN_PROGRESS"
  toUrlPiece CommonDriver.RR_COMPLETED = "RR_COMPLETED"
  toUrlPiece CommonDriver.RR_REJECTED = "RR_REJECTED"

instance ToParamSchema CommonDriver.EntityType

instance ToParamSchema CommonDriver.ReviewRequestType

instance ToParamSchema CommonDriver.ReviewRequestStatus
