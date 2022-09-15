{-# LANGUAGE TypeApplications #-}

module Tools.Auth.Verify (module Tools.Auth.Verify, module Reexport) where

import Beckn.Prelude
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Utils.Common
import Data.Singletons.TH
import Domain.Types.AccessMatrix as Reexport (ApiAccessType (..), ApiEntity (..))
import qualified Domain.Types.AccessMatrix as DMatrix
import qualified Domain.Types.Person as DP
import Domain.Types.Role as Reexport (DashboardAccessType (..))
import qualified Domain.Types.Role as DRole
import qualified Storage.Queries.AccessMatrix as QAccessMatrix
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Role as QRole
import Tools.Error
import Tools.Servant.HeaderAuth

-- These types are similar to DMatrix.ApiAccessLevel and DRole.DashboardAccessType, but used only on type level

data ApiAccessLevel at ae

data DashboardAccessLevel at

instance
  forall (at :: DMatrix.ApiAccessType) (ae :: DMatrix.ApiEntity).
  (SingI at, SingI ae) =>
  (VerificationPayload DMatrix.RequiredAccessLevel) (ApiAccessLevel at ae)
  where
  toPayloadType _ =
    DMatrix.RequiredApiAccessLevel $
      DMatrix.ApiAccessLevel
        { apiAccessType = fromSing (sing @at),
          apiEntity = fromSing (sing @ae)
        }

instance
  forall (at :: DRole.DashboardAccessType).
  SingI at =>
  (VerificationPayload DMatrix.RequiredAccessLevel) (DashboardAccessLevel at)
  where
  toPayloadType _ =
    DMatrix.RequiredDashboardAccessLevel (fromSing (sing @at))

-- TODO make tests for this logic
verifyAccessLevel :: EsqDBFlow m r => DMatrix.RequiredAccessLevel -> Id DP.Person -> m (Id DP.Person)
verifyAccessLevel requiredAccessLevel personId = do
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  case requiredAccessLevel of
    DMatrix.RequiredApiAccessLevel apiAccessLevel -> do
      mbAccessMatrixItem <- QAccessMatrix.findByRoleAndEntity person.roleId apiAccessLevel.apiEntity
      let userAccessType = maybe DMatrix.USER_NO_ACCESS (.userAccessType) mbAccessMatrixItem
      unless (checkUserAccess userAccessType apiAccessLevel.apiAccessType) $
        throwError AccessDenied
      pure person.id
    DMatrix.RequiredDashboardAccessLevel DRole.DASHBOARD_ADMIN -> do
      role <- QRole.findById person.roleId >>= fromMaybeM (RoleNotFound person.roleId.getId)
      if role.dashboardAccessType == DRole.DASHBOARD_ADMIN
        then pure person.id
        else throwError AccessDenied
    DMatrix.RequiredDashboardAccessLevel DRole.DASHBOARD_USER ->
      pure person.id

checkUserAccess :: DMatrix.UserAccessType -> ApiAccessType -> Bool
checkUserAccess DMatrix.USER_FULL_ACCESS _ = True
checkUserAccess DMatrix.USER_READ_ACCESS READ_ACCESS = True
checkUserAccess DMatrix.USER_WRITE_ACCESS WRITE_ACCESS = True
checkUserAccess _ _ = False
