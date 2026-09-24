{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.FarePolicyAmbulanceDetailsSlab where

import qualified Domain.Types.FarePolicy.Common
import qualified Domain.Types.FarePolicy.FarePolicySlabsDetails.FarePolicySlabsDetailsSlab
import qualified Domain.Types.FarePolicyAmbulanceDetailsSlab
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.FarePolicyAmbulanceDetailsSlab as Beam

instance FromTType' Beam.FarePolicyAmbulanceDetailsSlab Domain.Types.FarePolicyAmbulanceDetailsSlab.FarePolicyAmbulanceDetailsSlab where
  fromTType' (Beam.FarePolicyAmbulanceDetailsSlabT {..}) = do
    pure $
      Just
        Domain.Types.FarePolicyAmbulanceDetailsSlab.FarePolicyAmbulanceDetailsSlab
          { baseDistance = baseDistance,
            baseFare = baseFare,
            currency = currency,
            farePolicyId = farePolicyId,
            id = id,
            nightShiftCharge = nightShiftCharge,
            perKmRate = perKmRate,
            platformFeeInfo = ((,,) <$> platformFeeCharge <*> platformFeeCgst <*> platformFeeSgst) <&> (\(pfc, cg, sg) -> Domain.Types.FarePolicy.FarePolicySlabsDetails.FarePolicySlabsDetailsSlab.PlatformFeeInfo {platformFeeCharge = pfc, cgst = cg, sgst = sg}),
            vehicleAge = vehicleAge,
            waitingChargeInfo = ((,) <$> waitingCharge <*> freeWaitingTime) <&> (\(wc, ft) -> Domain.Types.FarePolicy.Common.WaitingChargeInfo {waitingCharge = wc, freeWaitingTime = ft})
          }

instance ToTType' Beam.FarePolicyAmbulanceDetailsSlab Domain.Types.FarePolicyAmbulanceDetailsSlab.FarePolicyAmbulanceDetailsSlab where
  toTType' (Domain.Types.FarePolicyAmbulanceDetailsSlab.FarePolicyAmbulanceDetailsSlab {..}) = do
    Beam.FarePolicyAmbulanceDetailsSlabT
      { Beam.baseDistance = baseDistance,
        Beam.baseFare = baseFare,
        Beam.currency = currency,
        Beam.farePolicyId = farePolicyId,
        Beam.id = id,
        Beam.nightShiftCharge = nightShiftCharge,
        Beam.perKmRate = perKmRate,
        Beam.platformFeeCgst = (.cgst) <$> platformFeeInfo,
        Beam.platformFeeCharge = (.platformFeeCharge) <$> platformFeeInfo,
        Beam.platformFeeSgst = (.sgst) <$> platformFeeInfo,
        Beam.vehicleAge = vehicleAge,
        Beam.freeWaitingTime = (.freeWaitingTime) <$> waitingChargeInfo,
        Beam.waitingCharge = (.waitingCharge) <$> waitingChargeInfo
      }
