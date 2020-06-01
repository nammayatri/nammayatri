{-# LANGUAGE OverloadedLabels #-}

module Product.Location where

import Beckn.Types.App
import qualified Beckn.Types.Storage.Location as SL
import Beckn.Utils.Common
import Data.Generics.Labels
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified Storage.Queries.RegistrationToken as QR
import qualified Storage.Redis.Queries as Redis
import Types.API.Location

updateLocation :: Text -> Maybe Text -> UpdateLocationReq -> FlowHandler UpdateLocationRes
updateLocation caseId token req = withFlowHandler $ do
  QR.verifyAuth token -- TODO: Move this verification to redis
    -- TODO: Add a driver and case check
  Redis.setKeyRedis caseId req
  return $ UpdateLocationRes "SUCCESS"

getLocation :: Text -> FlowHandler GetLocationRes
getLocation caseId = withFlowHandler $ do
  GetLocationRes <$> (Redis.getKeyRedis caseId)
