{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.FarePolicySlabsDetailsSlab where

import qualified Domain.Types.FarePolicy.Common
import qualified Domain.Types.FarePolicy.FarePolicySlabsDetails.FarePolicySlabsDetailsSlab
import qualified Domain.Types.FarePolicySlabsDetailsSlab
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.FarePolicySlabsDetailsSlab as Beam

instance FromTType' Beam.FarePolicySlabsDetailsSlab Domain.Types.FarePolicySlabsDetailsSlab.FarePolicySlabsDetailsSlab where
  fromTType' (Beam.FarePolicySlabsDetailsSlabT {..}) = do
    pure $
      Just
        Domain.Types.FarePolicySlabsDetailsSlab.FarePolicySlabsDetailsSlab
          { baseFare = Kernel.Types.Common.mkAmountWithDefault baseFareAmount baseFare,
            currency = Kernel.Prelude.fromMaybe Kernel.Types.Common.INR currency,
            distanceUnit = Kernel.Prelude.fromMaybe Kernel.Types.Common.Meter distanceUnit,
            farePolicyId = farePolicyId,
            id = id,
            nightShiftCharge = nightShiftCharge,
            platformFeeInfo = ((,,) <$> platformFeeCharge <*> platformFeeCgst <*> platformFeeSgst) <&> (\(pfc, cg, sg) -> Domain.Types.FarePolicy.FarePolicySlabsDetails.FarePolicySlabsDetailsSlab.PlatformFeeInfo {platformFeeCharge = pfc, cgst = cg, sgst = sg}),
            startDistance = startDistance,
            waitingChargeInfo = ((,) <$> waitingCharge <*> freeWatingTime) <&> (\(wc, ft) -> Domain.Types.FarePolicy.Common.WaitingChargeInfo {waitingCharge = wc, freeWaitingTime = ft})
          }

instance ToTType' Beam.FarePolicySlabsDetailsSlab Domain.Types.FarePolicySlabsDetailsSlab.FarePolicySlabsDetailsSlab where
  toTType' (Domain.Types.FarePolicySlabsDetailsSlab.FarePolicySlabsDetailsSlab {..}) = do
    Beam.FarePolicySlabsDetailsSlabT
      { Beam.baseFare = Kernel.Prelude.roundToIntegral baseFare,
        Beam.baseFareAmount = Kernel.Prelude.Just baseFare,
        Beam.currency = Kernel.Prelude.Just currency,
        Beam.distanceUnit = Kernel.Prelude.Just distanceUnit,
        Beam.farePolicyId = farePolicyId,
        Beam.id = id,
        Beam.nightShiftCharge = nightShiftCharge,
        Beam.platformFeeCgst = (.cgst) <$> platformFeeInfo,
        Beam.platformFeeCharge = (.platformFeeCharge) <$> platformFeeInfo,
        Beam.platformFeeSgst = (.sgst) <$> platformFeeInfo,
        Beam.startDistance = startDistance,
        Beam.freeWatingTime = (.freeWaitingTime) <$> waitingChargeInfo,
        Beam.waitingCharge = (.waitingCharge) <$> waitingChargeInfo
      }
