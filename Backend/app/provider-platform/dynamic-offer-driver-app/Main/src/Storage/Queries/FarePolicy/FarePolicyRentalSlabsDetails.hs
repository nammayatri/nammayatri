{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.FarePolicy.FarePolicyRentalSlabDetails where

import qualified Domain.Types.FarePolicy as DFP
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import qualified Kernel.Types.Id as KTI
import qualified Sequelize as Se
import qualified Storage.Beam.FarePolicy.FarePolicyRentalSlabDetails.FarePolicyRentalSlabDetailsSlab as BeamFPRSS

findAll' ::
  MonadFlow m =>
  Id DFP.FarePolicy ->
  m [BeamFPRSS.FullFarePolicyRentalSlabDetails]
findAll' (Id farePolicyId) = findAllWithOptionsKV [Se.Is BeamFPRSS.farePolicyId $ Se.Eq farePolicyId] (Se.Asc BeamFPRSS.baseDistance) Nothing Nothing

findById'' ::
  MonadFlow m =>
  Id DFP.FarePolicy ->
  m (Maybe BeamFPRSS.FullFarePolicyRentalSlabDetails)
findById'' (Id farePolicyId) = findAllWithKV [Se.Is BeamFPRSS.farePolicyId $ Se.Eq farePolicyId] <&> listToMaybe

deleteAll' :: MonadFlow m => Id DFP.FarePolicy -> m ()
deleteAll' (Id farePolicyId) = deleteWithKV [Se.Is BeamFPRSS.farePolicyId $ Se.Eq farePolicyId]

instance FromTType' BeamFPRSS.FarePolicyRentalSlabDetails BeamFPRSS.FullFarePolicyRentalSlabDetails where
  fromTType' BeamFPRSS.FarePolicyRentalSlabDetailsT {..} = do
    pure $
      Just
        ( KTI.Id farePolicyId,
          DFP.FPRSlabDetails
            { 
              baseDistance = baseDistance,
              baseDuration = baseDuration,
              kmAddedForEveryExtraHour = kmAddedForEveryExtraHour,
              extraRentalKmFare = extraRentalKmFare,
              extraRentalHoursFare = extraRentalHoursFare,
              baseFare = baseFare,
              waitingChargeInfo =
                ((,) <$> waitingCharge <*> freeWatingTime) <&> \(waitingCharge', freeWaitingTime') ->
                  DFP.WaitingChargeInfo
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

instance ToTType' BeamFPRSS.FarePolicyRentalSlabDetails BeamFPRSS.FullFarePolicyRentalSlabDetails where
  toTType' (KTI.Id farePolicyId, DFP.FPRSlabDetails {..}) =
    BeamFPRSS.FarePolicyRentalSlabDetailsT
      { 
        -- id = id,
        farePolicyId = farePolicyId,
        baseDistance = baseDistance,
        baseDuration = baseDuration,
        kmAddedForEveryExtraHour = kmAddedForEveryExtraHour,
        extraRentalKmFare = extraRentalKmFare,
        extraRentalHoursFare = extraRentalHoursFare,
        baseFare = baseFare,
        platformFeeCharge = DFP.platformFeeCharge <$> platformFeeInfo,
        platformFeeCgst = DFP.cgst <$> platformFeeInfo,
        platformFeeSgst = DFP.sgst <$> platformFeeInfo,
        waitingCharge = DFP.waitingCharge <$> waitingChargeInfo,
        nightShiftCharge = nightShiftCharge,
        freeWatingTime = DFP.freeWaitingTime <$> waitingChargeInfo
      }
