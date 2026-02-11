{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Storage.Queries.Role
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import qualified Database.Beam as B
import Domain.Types.Role as Role
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Sequelize as Se
import Storage.Beam.BeamFlow
import Storage.Beam.Common as SBC
import qualified Storage.Beam.Role as BeamR
import Storage.Queries.OrphanInstances.Role ()

create :: BeamFlow m r => Role -> m ()
create = createWithKV

findById :: BeamFlow m r => Id Role -> m (Maybe Role)
findById roleId = findOneWithKV [Se.Is BeamR.id $ Se.Eq $ getId roleId]

findByName :: BeamFlow m r => Text -> m (Maybe Role)
findByName name = findOneWithKV [Se.Is BeamR.name $ Se.Eq name]

findByDashboardAccessType :: BeamFlow m r => Role.DashboardAccessType -> m (Maybe Role)
findByDashboardAccessType dashboardAccessType =
  findOneWithKV [Se.Is BeamR.dashboardAccessType $ Se.Eq dashboardAccessType]

findAllInDashboardAccessType :: BeamFlow m r => [Role.DashboardAccessType] -> m [Role]
findAllInDashboardAccessType dashboardAccessTypes =
  findAllWithKV [Se.Is BeamR.dashboardAccessType $ Se.In dashboardAccessTypes]

findAllByLimitOffset :: BeamFlow m r => Maybe Integer -> Maybe Integer -> m [Role]
findAllByLimitOffset mbLimit mbOffset = do
  let limitVal = fromIntegral $ fromMaybe 10 mbLimit
      offsetVal = fromIntegral $ fromMaybe 0 mbOffset
  findAllWithOptionsKV [Se.Is BeamR.id $ Se.Not $ Se.Eq ""] (Se.Asc BeamR.name) (Just limitVal) (Just offsetVal)

findAllWithLimitOffset ::
  BeamFlow m r =>
  Maybe Integer ->
  Maybe Integer ->
  Maybe Text ->
  m [Role]
findAllWithLimitOffset mbLimit mbOffset mbSearchString = do
  let limitVal = fromMaybe 10 mbLimit
      offsetVal = fromMaybe 0 mbOffset
  dbConf <- getReplicaBeamConfig
  res <- L.runDB dbConf $
    L.findRows $
      B.select $
        B.limit_ limitVal $
          B.offset_ offsetVal $
            B.orderBy_ (\role -> B.desc_ role.name) $
              B.filter_'
                ( \role ->
                    maybe (B.sqlBool_ $ B.val_ True) (\searchStr -> B.sqlBool_ (role.name `B.like_` B.val_ ("%" <> searchStr <> "%"))) mbSearchString
                )
                do
                  B.all_ (SBC.role SBC.atlasDB)
  case res of
    Right res' -> do
      let m' = res'
      catMaybes <$> mapM fromTType' m'
    Left _ -> pure []

-- Do not use directly. Use cached version!
findParentRolesRecursively :: BeamFlow m r => Role.Role -> m [Role.Role]
findParentRolesRecursively role = findNextParentRoleRecursively role.parentRoleId []
  where
    findNextParentRoleRecursively :: BeamFlow m r => Maybe (Id Role.Role) -> [Role.Role] -> m [Role.Role]
    findNextParentRoleRecursively Nothing acc = pure $ reverse acc
    findNextParentRoleRecursively (Just parentId) acc = do
      -- detect cycle via already accumulated parents, including current role.id
      -- in current implementation role can't be parent of itself
      when (any (\r -> r.id == parentId) (role : acc)) $ do
        let cyclicParentIds = reverse $ parentId : (acc <&> (.id))
        logError $ "Cyclic parentRoleId reference detected in roles: rodeId: " <> role.id.getId <> "; roleName: " <> show role.name <> "; parentIds: " <> show (cyclicParentIds <&> (.getId))
      mbParent <- findById parentId
      case mbParent of
        Nothing -> pure $ reverse acc -- should we throw error?
        Just parentRole -> findNextParentRoleRecursively parentRole.parentRoleId (parentRole : acc)

-- by default parentRoleId can be DASHBOARD_ADMIN
updateById :: BeamFlow m r => Role.Role -> m ()
updateById role = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamR.name role.name,
      Se.Set BeamR.dashboardAccessType role.dashboardAccessType,
      Se.Set BeamR.parentRoleId $ getId <$> role.parentRoleId,
      Se.Set BeamR.description role.description,
      Se.Set BeamR.updatedAt now
    ]
    [ Se.Is BeamR.id $ Se.Eq $ getId role.id
    ]
