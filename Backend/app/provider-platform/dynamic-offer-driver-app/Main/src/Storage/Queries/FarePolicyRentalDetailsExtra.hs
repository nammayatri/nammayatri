{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FarePolicyRentalDetailsExtra where

import qualified Data.List.NonEmpty as NE
import qualified Domain.Types.FarePolicy as Domain
import qualified Domain.Types.FarePolicy.FarePolicyRentalDetails.FarePolicyRentalDetailsDistanceBuffer as FPRDB
import qualified Domain.Types.FarePolicy.FarePolicyRentalDetails.FarePolicyRentalDetailsPricingSlabs as FPRDPS
import qualified Domain.Types.FarePolicyRentalDetails as DTFPRD
import qualified Domain.Types.FarePolicyRentalDetailsDistanceBuffers as DSLRDB
import qualified Domain.Types.FarePolicyRentalDetailsPricingSlabs as DSLRDPS
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id as KTI
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FarePolicyRentalDetails as BeamFPRD
import qualified Storage.Beam.FarePolicyRentalDetailsDistanceBuffers as BeamFPRDDB
import qualified Storage.Beam.FarePolicyRentalDetailsPricingSlabs as BeamFPRDPS
import qualified Storage.Queries.FarePolicyRentalDetailsDistanceBuffers as QueriesFPRDB
import qualified Storage.Queries.FarePolicyRentalDetailsPricingSlabs as QueriesFPRDPS
import Storage.Queries.OrphanInstances.FarePolicyRentalDetails

type FullFarePolicyRentalDetails = (KTI.Id Domain.FarePolicy, Domain.FPRentalDetails)

findById' :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => KTI.Id Domain.FarePolicy -> m (Maybe FullFarePolicyRentalDetails)
findById' fpId@(KTI.Id farePolicyId') = do
  mResult :: Maybe DTFPRD.FarePolicyRentalDetails <- findOneWithKV [Se.Is BeamFPRD.farePolicyId $ Se.Eq farePolicyId']
  case mResult of
    Nothing -> pure Nothing
    Just DTFPRD.FarePolicyRentalDetails {..} -> do
      fullFPRDB <- QueriesFPRDB.findAll' fpId
      fPRDB <- fromMaybeM (InternalError "No distance buffer found for rental") (NE.nonEmpty fullFPRDB)
      fullFPRDPS <- QueriesFPRDPS.findAll' fpId
      fPRDPS <- fromMaybeM (InternalError "No pricing slab found for rental") (NE.nonEmpty fullFPRDPS)
      pure . Just $
        ( fpId,
          Domain.FPRentalDetails
            { distanceBuffers = snd <$> fPRDB,
              pricingSlabs = snd <$> fPRDPS,
              ..
            }
        )

toDSLType :: FullFarePolicyRentalDetails -> DTFPRD.FarePolicyRentalDetails
toDSLType (KTI.Id farePolicyId, Domain.FPRentalDetails {..}) =
  DTFPRD.FarePolicyRentalDetails {..}

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => FullFarePolicyRentalDetails -> m ()
create farePolicyRentalDetails = do
  let fpId = fst farePolicyRentalDetails
      details = snd farePolicyRentalDetails
  mapM_ (\FPRDB.FPRentalDetailsDistanceBuffers {..} -> QueriesFPRDB.create DSLRDB.FarePolicyRentalDetailsDistanceBuffers {farePolicyId = KTI.getId fpId, ..}) (NE.toList details.distanceBuffers)
  mapM_ (\FPRDPS.FPRentalDetailsPricingSlabs {..} -> QueriesFPRDPS.create DSLRDPS.FarePolicyRentalDetailsPricingSlabs {farePolicyId = KTI.getId fpId, ..}) (NE.toList details.pricingSlabs)
  createWithKV $ toDSLType farePolicyRentalDetails

delete :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => KTI.Id Domain.FarePolicy -> m ()
delete farePolicyId = do
  QueriesFPRDB.delete farePolicyId
  QueriesFPRDPS.delete farePolicyId
  deleteWithKV [Se.Is BeamFPRD.farePolicyId $ Se.Eq (KTI.getId farePolicyId)]

fromTTypeFarePolicyRentalDetails ::
  BeamFPRD.FarePolicyRentalDetails ->
  NonEmpty QueriesFPRDB.FullFarePolicyRentalDetailsDistanceBuffers ->
  NonEmpty QueriesFPRDPS.FullFarePolicyRentalDetailsPricingSlabs ->
  FullFarePolicyRentalDetails
fromTTypeFarePolicyRentalDetails BeamFPRD.FarePolicyRentalDetailsT {..} fPRDB fPRDPS =
  ( KTI.Id farePolicyId,
    Domain.FPRentalDetails
      { baseFare = mkAmountWithDefault baseFareAmount baseFare,
        perHourCharge = mkAmountWithDefault perHourChargeAmount perHourCharge,
        perExtraMinRate = mkAmountWithDefault perExtraMinRateAmount perExtraMinRate,
        perExtraKmRate = mkAmountWithDefault perExtraKmRateAmount perExtraKmRate,
        nightShiftCharge = nightShiftCharge,
        includedKmPerHr = includedKmPerHr,
        deadKmFare = deadKmFare,
        plannedPerKmRate = mkAmountWithDefault plannedPerKmRateAmount plannedPerKmRate,
        maxAdditionalKmsLimit = maxAdditionalKmsLimit,
        totalAdditionalKmsLimit = totalAdditionalKmsLimit,
        distanceBuffers = snd <$> fPRDB,
        pricingSlabs = snd <$> fPRDPS,
        waitingChargeInfo =
          ((,) <$> waitingCharge <*> freeWaitingTime) <&> \(waitingCharge', freeWaitingTime') ->
            Domain.WaitingChargeInfo
              { waitingCharge = waitingCharge',
                freeWaitingTime = freeWaitingTime'
              },
        currency = fromMaybe INR currency
      }
  )
