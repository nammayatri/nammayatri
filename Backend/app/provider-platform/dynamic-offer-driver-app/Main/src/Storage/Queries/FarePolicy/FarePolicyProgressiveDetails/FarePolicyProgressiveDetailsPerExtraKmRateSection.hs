{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.FarePolicy.FarePolicyProgressiveDetails.FarePolicyProgressiveDetailsPerExtraKmRateSection where

-- import Data.Text (pack)
import qualified Domain.Types.FarePolicy as DFP
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id as KTI
import Kernel.Utils.Common
import Lib.Utils (FromTType' (fromTType'), ToTType' (toTType'), deleteWithKV, findAllWithOptionsKV, findOneWithKV)
import Sequelize as Se
import qualified Storage.Beam.FarePolicy.FarePolicyProgressiveDetails.FarePolicyProgressiveDetailsPerExtraKmRateSection as BeamFPPDP
import Storage.Tabular.FarePolicy.FarePolicyProgressiveDetails.FarePolicyProgressiveDetailsPerExtraKmRateSection

findAll' ::
  ( Transactionable m,
    MonadThrow m,
    Log m
  ) =>
  Id DFP.FarePolicy ->
  DTypeBuilder m [FarePolicyProgressiveDetailsPerExtraKmRateSectionT]
findAll' farePolicyId = do
  Esq.findAll' $ do
    farePolicyProgressiveDetailsPerExtraKmFareSection <- from $ table @FarePolicyProgressiveDetailsPerExtraKmRateSectionT
    where_ $
      farePolicyProgressiveDetailsPerExtraKmFareSection ^. FarePolicyProgressiveDetailsPerExtraKmRateSectionFarePolicyId ==. val (toKey farePolicyId)
    orderBy [asc $ farePolicyProgressiveDetailsPerExtraKmFareSection ^. FarePolicyProgressiveDetailsPerExtraKmRateSectionStartDistance]
    return farePolicyProgressiveDetailsPerExtraKmFareSection

findById' :: (L.MonadFlow m, Log m) => KTI.Id DFP.FarePolicy -> m (Maybe FullFarePolicyProgressiveDetailsPerExtraKmRateSection)
findById' farePolicyId' = findOneWithKV [Se.Is BeamFPPDP.farePolicyId $ Se.Eq (getId farePolicyId')]

findAll ::
  ( L.MonadFlow m,
    Log m
  ) =>
  -- Id DFP.FarePolicy ->
  Id DFP.FarePolicy ->
  m [FullFarePolicyProgressiveDetailsPerExtraKmRateSection]
findAll farePolicyId = findAllWithOptionsKV [Se.Is BeamFPPDP.farePolicyId $ Se.Eq (getId farePolicyId)] (Se.Asc BeamFPPDP.startDistance) Nothing Nothing

deleteAll' :: Id DFP.FarePolicy -> FullEntitySqlDB ()
deleteAll' farePolicyId =
  Esq.delete' $ do
    farePolicyProgressiveDetailsPerExtraKmFareSection <- from $ table @FarePolicyProgressiveDetailsPerExtraKmRateSectionT
    where_ $
      farePolicyProgressiveDetailsPerExtraKmFareSection ^. FarePolicyProgressiveDetailsPerExtraKmRateSectionFarePolicyId ==. val (toKey farePolicyId)

deleteAll'' :: (L.MonadFlow m, Log m) => Id DFP.FarePolicy -> m ()
deleteAll'' (Id farePolicyId) = deleteWithKV [Se.Is BeamFPPDP.farePolicyId $ Se.Eq farePolicyId]

instance FromTType' BeamFPPDP.FarePolicyProgressiveDetailsPerExtraKmRateSection BeamFPPDP.FullFarePolicyProgressiveDetailsPerExtraKmRateSection where
  fromTType' BeamFPPDP.FarePolicyProgressiveDetailsPerExtraKmRateSectionT {..} = do
    pure $
      Just
        ( KTI.Id farePolicyId,
          DFP.FPProgressiveDetailsPerExtraKmRateSection
            { startDistance = startDistance,
              perExtraKmRate = perExtraKmRate
            }
        )

instance ToTType' BeamFPPDP.FarePolicyProgressiveDetailsPerExtraKmRateSection FullFarePolicyProgressiveDetailsPerExtraKmRateSection where
  toTType' (KTI.Id farePolicyId, DFP.FPProgressiveDetailsPerExtraKmRateSection {..}) =
    BeamFPPDP.FarePolicyProgressiveDetailsPerExtraKmRateSectionT
      { -- id = id,
        farePolicyId = farePolicyId,
        startDistance = startDistance,
        perExtraKmRate = perExtraKmRate
      }
