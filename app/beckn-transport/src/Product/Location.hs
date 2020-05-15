{-# LANGUAGE OverloadedLabels      #-}
module Product.Location where

import Beckn.Types.App
import Beckn.Utils.Common
import EulerHS.Prelude
import Types.API.Location
import Data.Generics.Labels
import qualified Beckn.Types.Storage.Location as SL
import qualified Storage.Queries.RegistrationToken as QR
import qualified Storage.Redis.Queries as Redis
import qualified EulerHS.Language as L

updateLocation :: Text -> Maybe Text -> UpdateLocationReq -> FlowHandler UpdateLocationRes
updateLocation caseId token req = withFlowHandler $ do
  QR.verifyAuth token
  Redis.setKeyRedis caseId req
  return $ UpdateLocationRes "SUCCESS"

getLocation :: Text -> FlowHandler GetLocationRes
getLocation caseId = withFlowHandler $ do
  GetLocationRes <$> (Redis.getKeyRedis caseId)