{-# LANGUAGE AllowAmbiguousTypes #-}

module Domain.Action.Internal.Auth where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.Person as DP
import Environment
import EulerHS.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Kernel.Utils.Common as Utils
import qualified Storage.Queries.RegistrationToken as QR
import Tools.Auth (validateToken)

newtype InternalResp = InternalResp
  { driverId :: Id DP.Person
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

internalAuth :: (HasField "locationTrackingServiceKey" AppEnv Text) => Maybe Text -> Maybe Text -> Maybe Text -> Flow InternalResp
internalAuth token apiKey merchantId = do
  locationTrackingServiceKey <- asks (.locationTrackingServiceKey)
  unless (apiKey == Just locationTrackingServiceKey) $ do
    throwError $ InvalidToken "Invalid api key"
  registraionDetails <-
    QR.findByToken (fromMaybe "" token)
      >>= Utils.fromMaybeM (InvalidRequest "Invalid token")
      >>= validateToken
  unless (registraionDetails.merchantId == fromMaybe "" merchantId) $ do
    throwError $ InvalidRequest "Invalid merchant id"
  pure $
    InternalResp
      { driverId = Id registraionDetails.entityId
      }
