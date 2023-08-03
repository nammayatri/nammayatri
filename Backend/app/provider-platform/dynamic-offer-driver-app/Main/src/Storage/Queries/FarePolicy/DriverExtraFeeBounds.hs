{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.FarePolicy.DriverExtraFeeBounds where

-- import qualified Domain.Types.FarePolicy as FarePolicy

-- import Kernel.Storage.Esqueleto as Esq

import Domain.Types.FarePolicy
import qualified Domain.Types.FarePolicy as DFP
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id as KTI
import qualified Sequelize as Se
import qualified Storage.Beam.FarePolicy.DriverExtraFeeBounds as BeamDEFB

-- create :: DFP.FullDriverExtraFeeBounds -> SqlDB ()
-- create = Esq.create

create :: (L.MonadFlow m, Log m) => DFP.FullDriverExtraFeeBounds -> m ()
create = createWithKV

-- findByFarePolicyIdAndStartDistance :: Transactionable m => Id DFP.FarePolicy -> Meters -> m (Maybe DFP.FullDriverExtraFeeBounds)
-- findByFarePolicyIdAndStartDistance farePolicyId startDistance = Esq.findOne $ do
--   farePolicy <- from $ table @DFP.DriverExtraFeeBoundsT
--   where_ $
--     farePolicy ^. DFP.DriverExtraFeeBoundsFarePolicyId ==. val (toKey farePolicyId)
--       &&. farePolicy ^. DFP.DriverExtraFeeBoundsStartDistance ==. val startDistance
--   pure farePolicy

findByFarePolicyIdAndStartDistance :: (L.MonadFlow m, Log m) => Id DFP.FarePolicy -> Meters -> m (Maybe DFP.FullDriverExtraFeeBounds)
findByFarePolicyIdAndStartDistance (Id farePolicyId) startDistance = findOneWithKV [Se.And [Se.Is BeamDEFB.farePolicyId $ Se.Eq farePolicyId, Se.Is BeamDEFB.startDistance $ Se.Eq startDistance]]

-- update :: Id DFP.FarePolicy -> Meters -> Money -> Money -> SqlDB ()
-- update farePolicyId startDistace minFee maxFee = do
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ DFP.DriverExtraFeeBoundsMinFee =. val minFee,
--         DFP.DriverExtraFeeBoundsMaxFee =. val maxFee
--       ]
--       tbl ^. DFP.DriverExtraFeeBoundsFarePolicyId ==. val (toKey farePolicyId)
--         &&. tbl ^. DFP.DriverExtraFeeBoundsStartDistance ==. val startDistace

update :: (L.MonadFlow m, Log m) => Id DFP.FarePolicy -> Meters -> Money -> Money -> m ()
update (Id farePolicyId) startDistance minFee maxFee =
  updateWithKV
    [Se.Set BeamDEFB.minFee minFee, Se.Set BeamDEFB.maxFee maxFee]
    [Se.And [Se.Is BeamDEFB.farePolicyId $ Se.Eq farePolicyId, Se.Is BeamDEFB.startDistance $ Se.Eq startDistance]]

findAll' ::
  ( L.MonadFlow m,
    MonadThrow m,
    Log m
  ) =>
  Id DFP.FarePolicy ->
  m [DFP.FullDriverExtraFeeBounds]
findAll' farePolicyId = findAllWithOptionsKV [Se.Is BeamDEFB.farePolicyId $ Se.Eq (getId farePolicyId)] (Se.Asc BeamDEFB.startDistance) Nothing Nothing

instance FromTType' BeamDEFB.DriverExtraFeeBounds DFP.FullDriverExtraFeeBounds where
  fromTType' BeamDEFB.DriverExtraFeeBoundsT {..} = do
    pure $
      Just
        ( KTI.Id farePolicyId,
          DFP.DriverExtraFeeBounds
            { startDistance = startDistance,
              minFee = minFee,
              maxFee = maxFee
            }
        )

instance ToTType' BeamDEFB.DriverExtraFeeBounds DFP.FullDriverExtraFeeBounds where
  toTType' (KTI.Id farePolicyId, DFP.DriverExtraFeeBounds {..}) =
    BeamDEFB.DriverExtraFeeBoundsT
      { id = Nothing,
        farePolicyId = farePolicyId,
        startDistance = startDistance,
        minFee = minFee,
        maxFee = maxFee
      }
