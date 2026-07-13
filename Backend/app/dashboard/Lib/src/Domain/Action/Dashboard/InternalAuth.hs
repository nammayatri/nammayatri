module Domain.Action.Dashboard.InternalAuth
  ( InternalAuthReq (..),
    InternalAuthResp (..),
    internalAuth,
  )
where

import qualified Domain.Types.Entity as DE
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.Person as DP
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.BeamFlow
import qualified Storage.Queries.Merchant as QMerchant
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Role as QRole
import qualified Tools.Auth.Common as Auth

data InternalAuthReq = InternalAuthReq {token :: Text}
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data InternalAuthResp = InternalAuthResp
  { personId :: Id DP.Person,
    merchantId :: ShortId DMerchant.Merchant,
    city :: City.City,
    roleName :: Text,
    firstName :: Text,
    lastName :: Text,
    entityId :: Maybe (Id DE.Entity)
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

-- Delegates the freshness check to `verifyPerson` so this stays in lock-step
-- with the Servant DashboardAuth path (same enabled flag, same inactivity
-- timeout, same Redis cache). All failure branches throw the same generic
-- message so callers can't enumerate why a token was rejected.
internalAuth ::
  (BeamFlow m r, Redis.HedisFlow m r, Auth.AuthFlow m r) =>
  InternalAuthReq ->
  m InternalAuthResp
internalAuth req = do
  (personId_, merchantId_, city_) <-
    catchAny (Auth.verifyPerson req.token) $ \e -> do
      logWarning $ "[internalAuth] verifyPerson failed: " <> show e -- infra failures distinguishable from invalid-token
      throwError invalidTokenError
  person <- QP.findById personId_ >>= fromMaybeM invalidTokenError
  role <- QRole.findById person.roleId >>= fromMaybeM invalidTokenError
  merchant <- QMerchant.findById merchantId_ >>= fromMaybeM invalidTokenError
  pure
    InternalAuthResp
      { personId = person.id,
        merchantId = merchant.shortId,
        city = city_,
        roleName = role.name,
        firstName = person.firstName,
        lastName = person.lastName,
        entityId = person.entityId
      }
  where
    invalidTokenError = InvalidRequest "Invalid or expired token"
