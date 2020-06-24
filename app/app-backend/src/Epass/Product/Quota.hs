module Epass.Product.Quota where

import Beckn.Types.Common
import qualified Beckn.Types.Storage.RegistrationToken as RegToken
import Beckn.Utils.Common
import Beckn.Utils.Extra (getCurrentTimeUTC)
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
import Epass.Utils.Storage
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant
import qualified Storage.Queries.RegistrationToken as RegToken

create :: RegToken -> CreateReq -> FlowHandler CreateRes
create regToken CreateReq {..} = withFlowHandler $ do
  token <- verifyToken regToken
  id <- generateGUID
  case (RegToken._entityType token) of
    RegToken.USER -> do
      let (userId) = RegToken._EntityId token
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
      now <- getCurrentTimeUTC
      return
        Storage.Quota
          { _id = id,
            _createdAt = now,
            _updatedAt = now,
            _TenantOrganizationId = TenantOrganizationId orgId,
            _info = Nothing,
            ..
          }

update :: RegToken -> QuotaId -> UpdateReq -> FlowHandler UpdateRes
update regToken id UpdateReq {..} = withFlowHandler $ do
  verifyToken regToken
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
  RegToken ->
  Maybe Limit ->
  Maybe Offset ->
  EntityType ->
  Text ->
  FlowHandler ListRes
list regToken mlimit moffset entityType entityId = withFlowHandler $
  do
    verifyToken regToken
    DB.findAllWithLimitOffset mlimit moffset entityType entityId
    >>= \case
      Left err -> L.throwException $ err500 {errBody = ("DBError: " <> show err)}
      Right v -> return $ ListRes v

get :: RegToken -> QuotaId -> FlowHandler GetRes
get regToken quotaId = withFlowHandler $
  do
    verifyToken regToken
    DB.findById quotaId
    >>= \case
      Right (Just user) -> return user
      Right Nothing -> L.throwException $ err400 {errBody = "Quota not found"}
      Left err -> L.throwException $ err500 {errBody = ("DBError: " <> show err)}
