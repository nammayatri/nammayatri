{-
  Copyright 2022-23, Juspay India Pvt Ltd
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.FarePolicy.FarePolicyAmbulanceDetailsSlab where

import Control.Lens ((^?), _head)
import qualified Domain.Types.FarePolicy as Domain
import qualified Domain.Types.FarePolicy.FarePolicyAmbulanceDetails as FPASlab
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Types.Id as KTI
import Kernel.Utils.Common hiding (id)
import Sequelize as Se
import Storage.Beam.FarePolicy.FarePolicyAmbulanceDetailsSlab as BeamFPAD

findById' :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => KTI.Id Domain.FarePolicy -> m [BeamFPAD.FullFarePolicyAmbulanceDetailsSlab]
findById' (KTI.Id farePolicyId') = findAllWithOptionsKV [Se.Is BeamFPAD.farePolicyId $ Se.Eq farePolicyId'] (Se.Asc BeamFPAD.vehicleAge) Nothing Nothing

getNextSlabId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => m Int
getNextSlabId = do
  rows <- findAllWithOptionsKV [Se.Is BeamFPAD.id $ Se.GreaterThanOrEq 0] (Se.Desc BeamFPAD.id) (Just 1) Nothing
  pure $ case rows ^? _head of
    Just (_, slab) -> FPASlab.id slab + 1
    Nothing -> 1

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => BeamFPAD.FullFarePolicyAmbulanceDetailsSlab -> m ()
create (farePolicyId, slab) = do
  nextId <- getNextSlabId
  createWithKV ((farePolicyId, slab {FPASlab.id = nextId}) :: BeamFPAD.FullFarePolicyAmbulanceDetailsSlab)

delete :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => KTI.Id Domain.FarePolicy -> m ()
delete farePolicyId = deleteWithKV [Se.Is BeamFPAD.farePolicyId $ Se.Eq (KTI.getId farePolicyId)]

instance FromTType' BeamFPAD.FarePolicyAmbulanceDetailsSlab BeamFPAD.FullFarePolicyAmbulanceDetailsSlab where
  fromTType' farePolicyAmbulanceDetailsSlab = do
    pure . Just $ fromTTypeFarePolicyAmbulanceDetails farePolicyAmbulanceDetailsSlab

fromTTypeFarePolicyAmbulanceDetails ::
  BeamFPAD.FarePolicyAmbulanceDetailsSlab ->
  BeamFPAD.FullFarePolicyAmbulanceDetailsSlab
fromTTypeFarePolicyAmbulanceDetails BeamFPAD.FarePolicyAmbulanceDetailsSlabT {..} =
  ( KTI.Id farePolicyId,
    Domain.FPAmbulanceDetailsSlab
      { waitingChargeInfo =
          ((,) <$> waitingCharge <*> freeWaitingTime) <&> \(waitingCharge', freeWaitingTime') ->
            Domain.WaitingChargeInfo
              { waitingCharge = waitingCharge',
                freeWaitingTime = freeWaitingTime'
              },
        platformFeeInfo =
          ((,,) <$> platformFeeCharge <*> platformFeeCgst <*> platformFeeSgst) <&> \(platformFeeCharge', platformFeeCgst', platformFeeSgst') ->
            Domain.PlatformFeeInfo
              { platformFeeCharge = platformFeeCharge',
                cgst = platformFeeCgst',
                sgst = platformFeeSgst'
              },
        ..
      }
  )

instance ToTType' BeamFPAD.FarePolicyAmbulanceDetailsSlab BeamFPAD.FullFarePolicyAmbulanceDetailsSlab where
  toTType' (KTI.Id farePolicyId, Domain.FPAmbulanceDetailsSlab {..}) =
    BeamFPAD.FarePolicyAmbulanceDetailsSlabT
      { platformFeeCharge = Domain.platformFeeCharge <$> (platformFeeInfo :: Maybe Domain.PlatformFeeInfo),
        platformFeeCgst = (.cgst) <$> (platformFeeInfo :: Maybe Domain.PlatformFeeInfo),
        platformFeeSgst = (.sgst) <$> (platformFeeInfo :: Maybe Domain.PlatformFeeInfo),
        waitingCharge = (.waitingCharge) <$> waitingChargeInfo,
        nightShiftCharge = nightShiftCharge,
        freeWaitingTime = (.freeWaitingTime) <$> waitingChargeInfo,
        ..
      }
