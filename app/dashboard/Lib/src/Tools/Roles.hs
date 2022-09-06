{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Tools.Roles where

import Beckn.Prelude
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Utils.Common
import Data.Singletons.TH
import Domain.Types.Person as DP
import qualified Storage.Queries.Person as QPerson
import Tools.Servant.HeaderAuth

data ApiAccessType = READ_ACCESS | WRITE_ACCESS

genSingletons [''ApiAccessType]

data UserAccessType = USER_READ_ACCESS | USER_WRITE_ACCESS | USER_FULL_ACCESS | USER_NO_ACCESS

data ApiEntity = CUSTOMERS | DRIVERS | RIDES | MONITORING

genSingletons [''ApiEntity]

-- AccessLevel is used only on type level, ApiAccessLevel for working with real data
data ApiAccessLevel = ApiAccessLevel
  { apiAccessType :: ApiAccessType,
    apiEntity :: ApiEntity
  }

data AccessLevel at ae

instance
  forall (at :: ApiAccessType) (ae :: ApiEntity).
  (SingI at, SingI ae) =>
  (VerificationPayload ApiAccessLevel) (AccessLevel at ae)
  where
  toPayloadType _ =
    ApiAccessLevel
      { apiAccessType = fromSing (sing @at),
        apiEntity = fromSing (sing @ae)
      }

verifyAccessLevel :: EsqDBFlow m r => ApiAccessLevel -> Id DP.Person -> m (Id DP.Person)
verifyAccessLevel requiredAccessLevel personId = do
  person <- QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  let userAccessType = accessMatrix person.role requiredAccessLevel.apiEntity
  unless (checkUserAccess userAccessType requiredAccessLevel.apiAccessType) $
    throwError AccessDenied
  pure person.id

checkUserAccess :: UserAccessType -> ApiAccessType -> Bool
checkUserAccess USER_FULL_ACCESS _ = True
checkUserAccess USER_READ_ACCESS READ_ACCESS = True
checkUserAccess USER_WRITE_ACCESS WRITE_ACCESS = True
checkUserAccess _ _ = False

-- TODO move access matrix to DB
accessMatrix :: DP.Role -> ApiEntity -> UserAccessType
accessMatrix role apiEntity =
  case (role, apiEntity) of
    (DP.CUSTOMER, RIDES) -> USER_READ_ACCESS
    (DP.DRIVER, RIDES) -> USER_READ_ACCESS
    (DP.JUSPAY_OPS, RIDES) -> USER_FULL_ACCESS
    (DP.JUSPAY_OPS, MONITORING) -> USER_FULL_ACCESS
    (DP.JUSPAY_OPS, _) -> USER_WRITE_ACCESS
    (DP.JUSPAY_ADMIN, _) -> USER_FULL_ACCESS
    (DP.CUSTOMER_SERVICE, RIDES) -> USER_FULL_ACCESS
    (DP.CUSTOMER_SERVICE, MONITORING) -> USER_WRITE_ACCESS
    (DP.CUSTOMER_SERVICE, _) -> USER_READ_ACCESS
    (_, _) -> USER_NO_ACCESS
