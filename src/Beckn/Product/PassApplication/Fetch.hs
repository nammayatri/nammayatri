module Beckn.Product.PassApplication.Fetch where

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

listPassApplication ::
  Maybe Text
  -> Maybe Int
  -> Maybe Int
  -> [Status]
  -> [PassType]
  -> FlowHandler ListPassApplicationRes
listPassApplication regToken offsetM limitM status passType = withFlowHandler $ do
  verifyToken regToken
  DB.findAllWithLimitOffsetWhere status passType limitM offsetM
  >>= \case
      Left err -> L.throwException $ err500 {errBody = ("DBError: " <> show err)}
      Right v -> return $ ListPassApplicationRes v

getPassApplicationById :: Maybe Text -> PassApplicationId -> FlowHandler PassApplicationRes
getPassApplicationById regToken applicationId = withFlowHandler $ do
  verifyToken regToken
  DB.findById applicationId
  >>= \case
    Right (Just v) -> return $ PassApplicationRes v
    Right Nothing -> L.throwException $ err400 {errBody = "Pass Application not found"}
    Left err -> L.throwException $ err500 {errBody = ("DBError: " <> show err)}
