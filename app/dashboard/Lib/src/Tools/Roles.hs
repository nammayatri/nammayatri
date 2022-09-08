{-# LANGUAGE TemplateHaskell #-}

module Tools.Roles where

import Beckn.Prelude
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Utils.Common
import Data.Singletons.TH
import Domain.Types.Person as DP
import qualified Storage.Queries.Person as QPerson

-------- Possible user access levels for helper API --------

data UserAccessType = USER_READ_ACCESS | USER_WRITE_ACCESS | USER_FULL_ACCESS | USER_NO_ACCESS

-------- Required access levels for helper api --------

data ApiAccessType = READ_ACCESS | WRITE_ACCESS

genSingletons [''ApiAccessType]

data ApiEntity = CUSTOMERS | DRIVERS | RIDES | MONITORING

genSingletons [''ApiEntity]

data ApiAccessLevel = ApiAccessLevel
  { apiAccessType :: ApiAccessType,
    apiEntity :: ApiEntity
  }

-------- Required access levels for dashboard api --------

data DashboardAccessType = DASHBOARD_USER | DASHBOARD_ADMIN

genSingletons [''DashboardAccessType]

-------- Required access levels for any api --------

data RequiredAccessLevel = RequiredApiAccessLevel ApiAccessLevel | RequiredDashboardAccessLevel DashboardAccessType

-- TODO make tests for this logic
verifyAccessLevel :: EsqDBFlow m r => RequiredAccessLevel -> Id DP.Person -> m (Id DP.Person)
verifyAccessLevel requiredAccessLevel personId = do
  person <- QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  case requiredAccessLevel of
    RequiredApiAccessLevel apiAccessLevel -> do
      let userAccessType = accessMatrix person.role apiAccessLevel.apiEntity
      unless (checkUserAccess userAccessType apiAccessLevel.apiAccessType) $
        throwError AccessDenied
      pure person.id
    RequiredDashboardAccessLevel DASHBOARD_ADMIN ->
      if person.role == JUSPAY_ADMIN
        then pure person.id
        else throwError AccessDenied
    RequiredDashboardAccessLevel DASHBOARD_USER ->
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
