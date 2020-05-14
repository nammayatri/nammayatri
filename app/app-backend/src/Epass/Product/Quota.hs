module Epass.Product.Quota where

import qualified Beckn.Types.Storage.RegistrationToken as RegToken
import Data.Aeson
import Data.Default
import Data.Time
import qualified Database.Beam.Schema.Tables as B
import qualified Epass.Storage.Queries.Quota as DB
import qualified Epass.Storage.Queries.User as User
import Epass.Types.API.Quota
import Epass.Types.App
import Epass.Types.Common
import Epass.Types.Storage.Quota as Storage
import qualified Epass.Types.Storage.User as User
import Epass.Utils.Common
import Epass.Utils.Routes
import Epass.Utils.Storage
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant
import qualified Storage.Queries.RegistrationToken as RegToken

create :: Maybe RegistrationTokenText -> CreateReq -> FlowHandler CreateRes
create mRegToken CreateReq {..} = withFlowHandler $ do
  regToken <- verifyToken mRegToken
  id <- generateGUID
  case (RegToken._entityType regToken) of
    RegToken.USER -> do
      let (userId) = RegToken._EntityId regToken
      user <- (User.findById $ UserId userId)
      let (OrganizationId orgId) = User._OrganizationId user
      quota <- quotaRec id orgId
      DB.create quota
      eres <- DB.findById id
      case eres of
        Right (Just quotaDb) -> return $ CreateRes quotaDb
        _ -> L.throwException $ err500 {errBody = "Could not create Quota"}
    RegToken.CUSTOMER -> throwUnauthorized
  where
    throwUnauthorized = L.throwException $ err401 {errBody = "Unauthorized"}
    quotaRec id orgId = do
      now <- getCurrTime
      return
        Storage.Quota
          { _id = id,
            _createdAt = now,
            _updatedAt = now,
            _TenantOrganizationId = TenantOrganizationId orgId,
            _info = Nothing,
            ..
          }

update :: Maybe RegistrationTokenText -> QuotaId -> UpdateReq -> FlowHandler UpdateRes
update mRegToken id UpdateReq {..} = withFlowHandler $ do
  verifyToken mRegToken
  eres <- DB.update id _maxAllowed _startTime _endTime
  case eres of
    Left err -> L.throwException $ err500 {errBody = ("DBError: " <> show err)}
    Right _ ->
      DB.findById id
        >>= \case
          Right (Just v) -> return $ UpdateRes v
          Right Nothing -> L.throwException $ err400 {errBody = "Quota not found"}
          Left err -> L.throwException $ err500 {errBody = ("DBError: " <> show err)}

list ::
  Maybe RegistrationTokenText ->
  Maybe Limit ->
  Maybe Offset ->
  EntityType ->
  Text ->
  FlowHandler ListRes
list mRegToken mlimit moffset entityType entityId = withFlowHandler $
  do
    verifyToken mRegToken
    DB.findAllWithLimitOffset mlimit moffset entityType entityId
    >>= \case
      Left err -> L.throwException $ err500 {errBody = ("DBError: " <> show err)}
      Right v -> return $ ListRes v

get :: Maybe Text -> QuotaId -> FlowHandler GetRes
get mRegToken quotaId = withFlowHandler $
  do
    verifyToken mRegToken
    DB.findById quotaId
    >>= \case
      Right (Just user) -> return user
      Right Nothing -> L.throwException $ err400 {errBody = "Quota not found"}
      Left err -> L.throwException $ err500 {errBody = ("DBError: " <> show err)}
