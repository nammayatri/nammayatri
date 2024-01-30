{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.FarePolicy.FarePolicySlabsDetails.FarePolicySlabsDetailsSlab where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Merchant as DPM
import qualified Domain.Types.FarePolicy as DFP
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import qualified Kernel.Types.Id as KTI
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.FarePolicy.FarePolicySlabDetails.FarePolicySlabDetailsSlab as BeamFPSS

findAll' ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DFP.FarePolicy ->
  m [BeamFPSS.FullFarePolicySlabsDetailsSlab]
findAll' (Id farePolicyId) = findAllWithOptionsKV [Se.Is BeamFPSS.farePolicyId $ Se.Eq farePolicyId] (Se.Asc BeamFPSS.startDistance) Nothing Nothing

findById'' ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DFP.FarePolicy ->
  m (Maybe BeamFPSS.FullFarePolicySlabsDetailsSlab)
findById'' (Id farePolicyId) = findAllWithKV [Se.Is BeamFPSS.farePolicyId $ Se.Eq farePolicyId] <&> listToMaybe

deleteAll' :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DFP.FarePolicy -> m ()
deleteAll' (Id farePolicyId) = deleteWithKV [Se.Is BeamFPSS.farePolicyId $ Se.Eq farePolicyId]

instance FromTType' BeamFPSS.FarePolicySlabsDetailsSlab BeamFPSS.FullFarePolicySlabsDetailsSlab where
  fromTType' BeamFPSS.FarePolicySlabsDetailsSlabT {..} = do
    pure $
      Just
        ( KTI.Id farePolicyId,
          DFP.FPSlabsDetailsSlab
            { startDistance = startDistance,
              baseFare = baseFare,
              waitingChargeInfo =
                ((,) <$> waitingCharge <*> freeWatingTime) <&> \(waitingCharge', freeWaitingTime') ->
                  DPM.WaitingChargeInfo
                    { waitingCharge = waitingCharge',
                      freeWaitingTime = freeWaitingTime'
                    },
              nightShiftCharge = nightShiftCharge,
              platformFeeInfo =
                ((,,) <$> platformFeeCharge <*> platformFeeCgst <*> platformFeeSgst) <&> \(platformFeeCharge', platformFeeCgst', platformFeeSgst') ->
                  DFP.PlatformFeeInfo
                    { platformFeeCharge = platformFeeCharge',
                      cgst = platformFeeCgst',
                      sgst = platformFeeSgst'
                    }
            }
        )

instance ToTType' BeamFPSS.FarePolicySlabsDetailsSlab BeamFPSS.FullFarePolicySlabsDetailsSlab where
  toTType' (KTI.Id farePolicyId, DFP.FPSlabsDetailsSlab {..}) =
    BeamFPSS.FarePolicySlabsDetailsSlabT
      { id = Nothing,
        farePolicyId = farePolicyId,
        startDistance = startDistance,
        baseFare = baseFare,
        platformFeeCharge = DFP.platformFeeCharge <$> platformFeeInfo,
        platformFeeCgst = DFP.cgst <$> platformFeeInfo,
        platformFeeSgst = DFP.sgst <$> platformFeeInfo,
        waitingCharge = (.waitingCharge) <$> waitingChargeInfo,
        nightShiftCharge = nightShiftCharge,
        freeWatingTime = (.freeWaitingTime) <$> waitingChargeInfo
      }
