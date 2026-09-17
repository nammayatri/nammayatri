{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.ResourceScope where

import Data.List (nub)
import qualified Domain.Types.Capability as DC
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.Person as DP
import qualified Domain.Types.ResourceScope as DRS
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (Success))
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.BeamFlow
import qualified Storage.Queries.AccessAudit as QAudit
import qualified Storage.Queries.Merchant as QMerchant
import qualified Storage.Queries.MerchantAccess as QAccess
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.PersonResourceAccess as QPRA
import Tools.Auth
import Tools.Auth.Capability (isSuperAdmin)
import Tools.Error

-- Layer C management surface. `resourceType` is a closed enum (DRS.ResourceType);
-- `resourceId` is opaque Text (route code, special-location id, zone name, …).
-- Reset-then-insert so a
-- person's stored set for a (merchant, city, type) is exactly what was sent.
-- Scope is opt-in: no rows = unscoped = allow-all; a DRS.wildcardResourceId ("*")
-- row is the same allow-all; a specific list restricts. /user/resourceScope is
-- what control-center analytics + the ops gate read.

--------------------------------------------------------------------- types

data UserResourceScopeEntry = UserResourceScopeEntry
  { resourceType :: DRS.ResourceType,
    resourceIds :: [Text]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

-- | The caller's own scope for the SESSION's merchant + city, grouped by type.
newtype UserResourceScopeRes = UserResourceScopeRes
  { list :: [UserResourceScopeEntry]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data AssignResourceAccessReq = AssignResourceAccessReq
  { merchantShortId :: ShortId DMerchant.Merchant,
    operatingCity :: City.City,
    resourceType :: DRS.ResourceType,
    resourceIds :: [Text]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data ResetResourceAccessReq = ResetResourceAccessReq
  { merchantShortId :: ShortId DMerchant.Merchant,
    operatingCity :: City.City,
    resourceType :: DRS.ResourceType
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data ResourceAccessRow = ResourceAccessRow
  { merchantShortId :: ShortId DMerchant.Merchant,
    operatingCity :: City.City,
    resourceType :: DRS.ResourceType,
    resourceId :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

newtype PersonResourceAccessRes = PersonResourceAccessRes
  { list :: [ResourceAccessRow]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

------------------------------------------------------------------- helpers

audit :: BeamFlow m r => TokenInfo -> Text -> Text -> Maybe Text -> Maybe Text -> m ()
audit tokenInfo action targetId beforeValue afterValue = do
  now <- getCurrentTime
  auditId <- generateGUID
  QAudit.create $
    DC.AccessAudit
      { id = auditId,
        actorId = Just tokenInfo.personId,
        action = action,
        targetType = "person",
        targetId = Just targetId,
        beforeValue = beforeValue,
        afterValue = afterValue,
        reason = Nothing,
        createdAt = now
      }

assertAdminMayManageMOC :: BeamFlow m r => TokenInfo -> Id DMerchant.Merchant -> City.City -> m ()
assertAdminMayManageMOC tokenInfo mId city =
  unless (mId == tokenInfo.merchantId && city == tokenInfo.city) $
    unlessM (isSuperAdmin tokenInfo.personId) $
      throwError AccessDenied

------------------------------------------------------------------ handlers

getUserResourceScope :: BeamFlow m r => TokenInfo -> m UserResourceScopeRes
getUserResourceScope tokenInfo = do
  rows <- QPRA.findAllByPersonId tokenInfo.personId
  -- Resource scope is per (merchant, city); narrow to the session's MOC, then
  -- group by whatever resource types are present (no hardcoded type list, so a
  -- new resource kind appears with no code change).
  let mine = filter (\r -> r.merchantId == tokenInfo.merchantId && r.operatingCity == tokenInfo.city) rows
      types = nub (map (.resourceType) mine)
      entryFor rt = UserResourceScopeEntry rt [r.resourceId | r <- mine, r.resourceType == rt]
  pure $ UserResourceScopeRes $ map entryFor types

getPersonResourceAccess :: BeamFlow m r => TokenInfo -> Id DP.Person -> m PersonResourceAccessRes
getPersonResourceAccess tokenInfo personId = do
  void $ QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  rows <- QPRA.findAllByPersonId personId
  sa <- isSuperAdmin tokenInfo.personId
  let visible =
        if sa
          then rows
          else filter (\r -> r.merchantId == tokenInfo.merchantId && r.operatingCity == tokenInfo.city) rows
  -- Resolve each merchant's short id once (one findById per DISTINCT merchant,
  -- not per row) — a person can hold many rows across few merchants. Falls back
  -- to the raw id only for an orphaned row whose merchant was deleted.
  shortIdByMerchant <-
    forM (nub (map (.merchantId) visible)) $ \mId -> do
      mbMerchant <- QMerchant.findById mId
      pure (mId, maybe (ShortId mId.getId) (.shortId) mbMerchant)
  let shortIdOf mId = fromMaybe (ShortId mId.getId) (lookup mId shortIdByMerchant)
  pure $
    PersonResourceAccessRes $
      map
        (\r -> ResourceAccessRow {merchantShortId = shortIdOf r.merchantId, operatingCity = r.operatingCity, resourceType = r.resourceType, resourceId = r.resourceId})
        visible

-- | Reset-then-insert: the person's rows for this (merchant, city, type) become
-- exactly `resourceIds`. [] clears all rows → unrestricted (allow-all);
-- [DRS.wildcardResourceId] is the explicit allow-all; a specific list restricts.
assignResourceAccess :: BeamFlow m r => TokenInfo -> Id DP.Person -> AssignResourceAccessReq -> m APISuccess
assignResourceAccess tokenInfo personId req = do
  void $ QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  merchant <- QMerchant.findByShortId req.merchantShortId >>= fromMaybeM (MerchantDoesNotExist req.merchantShortId.getShortId)
  assertAdminMayManageMOC tokenInfo merchant.id req.operatingCity
  -- The grant is meaningless (and a privilege-escalation vector) unless the
  -- target person actually has access to this merchant + operating city. Reject
  -- a (merchant, city) the person isn't provisioned on via merchant_access.
  void $
    QAccess.findByPersonIdAndMerchantIdAndCity personId merchant.id req.operatingCity
      >>= fromMaybeM (InvalidRequest "Target person has no access to this merchant / operating city.")
  QPRA.deleteByPersonMerchantCityType personId merchant.id req.operatingCity req.resourceType
  now <- getCurrentTime
  forM_ (nub req.resourceIds) $ \resourceId -> do
    guid <- generateGUID
    QPRA.create $
      DRS.PersonResourceAccess
        { id = guid,
          personId,
          merchantId = merchant.id,
          operatingCity = req.operatingCity,
          resourceType = req.resourceType,
          resourceId,
          createdAt = now
        }
  audit
    tokenInfo
    "PERSON_RESOURCE_ACCESS_ASSIGN"
    personId.getId
    Nothing
    (Just $ show req.resourceType <> " @ " <> req.merchantShortId.getShortId <> "/" <> show req.operatingCity <> " = " <> show (nub req.resourceIds))
  pure Success

resetResourceAccess :: BeamFlow m r => TokenInfo -> Id DP.Person -> ResetResourceAccessReq -> m APISuccess
resetResourceAccess tokenInfo personId req = do
  void $ QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  merchant <- QMerchant.findByShortId req.merchantShortId >>= fromMaybeM (MerchantDoesNotExist req.merchantShortId.getShortId)
  assertAdminMayManageMOC tokenInfo merchant.id req.operatingCity
  QPRA.deleteByPersonMerchantCityType personId merchant.id req.operatingCity req.resourceType
  audit
    tokenInfo
    "PERSON_RESOURCE_ACCESS_RESET"
    personId.getId
    (Just $ show req.resourceType <> " @ " <> req.merchantShortId.getShortId <> "/" <> show req.operatingCity)
    Nothing
  pure Success
