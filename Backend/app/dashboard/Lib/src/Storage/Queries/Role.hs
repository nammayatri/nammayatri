{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Role where

import qualified Database.Beam as B
import Domain.Types.Role as Role
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Sequelize as Se
import Storage.Beam.BeamFlow
import Storage.Beam.Common as SBC
import qualified Storage.Beam.Role as BeamR

create :: BeamFlow m r => Role -> m ()
create = createWithKV

findById :: BeamFlow m r => Id Role -> m (Maybe Role)
findById roleId = findOneWithKV [Se.Is BeamR.id $ Se.Eq $ getId roleId]

findByName :: BeamFlow m r => Text -> m (Maybe Role)
findByName name = findOneWithKV [Se.Is BeamR.name $ Se.Eq name]

findByDashboardAccessType :: BeamFlow m r => Role.DashboardAccessType -> m (Maybe Role)
findByDashboardAccessType dashboardAccessType =
  findOneWithKV [Se.Is BeamR.dashboardAccessType $ Se.Eq dashboardAccessType]

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

instance FromTType' BeamR.Role Role.Role where
  fromTType' BeamR.RoleT {..} = do
    return $
      Just
        Role.Role
          { id = Id id,
            ..
          }

instance ToTType' BeamR.Role Role.Role where
  toTType' Role.Role {..} =
    BeamR.RoleT
      { id = getId id,
        ..
      }
