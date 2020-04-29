module Beckn.Product.PassApplication.Update where

import qualified Beckn.Data.Accessor                   as Accessor
import qualified Beckn.Storage.Queries.PassApplication as DB
import           Beckn.Types.API.PassApplication
import           Beckn.Types.App
import           Beckn.Types.Common
import qualified Beckn.Types.Common                    as Location (Location (..),
                                                                    LocationType)
import           Beckn.Types.Storage.PassApplication
import qualified Beckn.Types.Storage.RegistrationToken as RegistrationToken
import           Beckn.Utils.Common
import           Beckn.Utils.Routes
import           Beckn.Utils.Storage
import           Data.Aeson
import qualified EulerHS.Language                      as L
import           EulerHS.Prelude
import           Servant

updatePassApplication ::
  Maybe Text ->
  PassApplicationId ->
  UpdatePassApplicationReq ->
  FlowHandler PassApplicationRes
updatePassApplication regToken passApplicationId UpdatePassApplicationReq{..} = withFlowHandler $ do
  verifyToken regToken
  eres <- DB.update passApplicationId _status _approvedCount _remarks
  case eres of
    Left err -> L.throwException $ err500 {errBody = ("DBError: " <> show err)}
    Right _ ->
      DB.findById passApplicationId
      >>= \case
        Right (Just v) -> return $ PassApplicationRes v
        Right Nothing -> L.throwException $ err400 {errBody = "Pass Application not found"}
        Left err -> L.throwException $ err500 {errBody = ("DBError: " <> show err)}
