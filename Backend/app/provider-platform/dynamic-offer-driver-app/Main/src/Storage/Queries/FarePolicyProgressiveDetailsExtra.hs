{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FarePolicyProgressiveDetailsExtra where

import qualified Data.List.NonEmpty as NE
import qualified Domain.Types.FarePolicy as Domain
import qualified Domain.Types.FarePolicy.FarePolicyProgressiveDetails.FarePolicyProgressiveDetailsPerExtraKmRateSection as FPPEKRS
import qualified Domain.Types.FarePolicyProgressiveDetails as DTFPPD
import qualified Domain.Types.FarePolicyProgressiveDetailsPerExtraKmRateSection as DSLPEKRS
import qualified Domain.Types.FullFarePolicyProgressiveDetailsPerMinRateSection as FullMinDomain
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id as KTI
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FarePolicyProgressiveDetails as BeamFPPD
import qualified Storage.Beam.FarePolicyProgressiveDetailsPerExtraKmRateSection as BeamFPPDP
import qualified Storage.Queries.FarePolicy.FarePolicyProgressiveDetails.FarePolicyProgressiveDetailsPerMinRateSection as QueriesFPMin
import qualified Storage.Queries.FarePolicyProgressiveDetailsPerExtraKmRateSection as QueriesFPPDP
import qualified Storage.Queries.FullFarePolicyProgressiveDetailsPerMinRateSection as QueriesFullFPMin
import Storage.Queries.OrphanInstances.FarePolicyProgressiveDetails

type FullFarePolicyProgressiveDetails = (KTI.Id Domain.FarePolicy, Domain.FPProgressiveDetails)

findById' :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => KTI.Id Domain.FarePolicy -> m (Maybe FullFarePolicyProgressiveDetails)
findById' fpId@(KTI.Id farePolicyId') = do
  mResult :: Maybe DTFPPD.FarePolicyProgressiveDetails <- findOneWithKV [Se.Is BeamFPPD.farePolicyId $ Se.Eq farePolicyId']
  case mResult of
    Nothing -> pure Nothing
    Just DTFPPD.FarePolicyProgressiveDetails {..} -> do
      fullFPPDP <- QueriesFPPDP.findAll' fpId
      fullMinFP <- NE.nonEmpty <$> QueriesFPMin.findAll fpId
      fPPDP <- fromMaybeM (InternalError "FromLocation not found") (NE.nonEmpty fullFPPDP)
      pure . Just $
        ( fpId,
          Domain.FPProgressiveDetails
            { perExtraKmRateSections = snd <$> fPPDP,
              perMinRateSections = fullMinFP,
              ..
            }
        )

toDSLType :: FullFarePolicyProgressiveDetails -> DTFPPD.FarePolicyProgressiveDetails
toDSLType (KTI.Id farePolicyId, Domain.FPProgressiveDetails {..}) =
  DTFPPD.FarePolicyProgressiveDetails {..}

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => FullFarePolicyProgressiveDetails -> m ()
create farePolicyProgressiveDetails = do
  let fpId = fst farePolicyProgressiveDetails
      details = snd farePolicyProgressiveDetails
  mapM_ (\FPPEKRS.FPProgressiveDetailsPerExtraKmRateSection {..} -> QueriesFPPDP.create DSLPEKRS.FarePolicyProgressiveDetailsPerExtraKmRateSection {farePolicyId = KTI.getId fpId, ..}) (NE.toList details.perExtraKmRateSections)
  whenJust details.perMinRateSections $ \sections -> do
    now <- getCurrentTime
    let fullSections = mkFullPerMinRateSections (KTI.getId fpId) details.currency now sections
    QueriesFullFPMin.createMany fullSections
  createWithKV $ toDSLType farePolicyProgressiveDetails
  where
    mkFullPerMinRateSections :: Text -> Currency -> UTCTime -> NonEmpty Domain.FPProgressiveDetailsPerMinRateSection -> [FullMinDomain.FullFarePolicyProgressiveDetailsPerMinRateSection]
    mkFullPerMinRateSections fpIdText currency now sections =
      map
        ( \s ->
            FullMinDomain.FullFarePolicyProgressiveDetailsPerMinRateSection
              { farePolicyId = fpIdText,
                rideDurationInMin = s.rideDurationInMin,
                perMinRate = s.perMinRate.amount,
                currency = currency,
                createdAt = now,
                updatedAt = now
              }
        )
        (NE.toList sections)

delete :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => KTI.Id Domain.FarePolicy -> m ()
delete farePolicyId = do
  QueriesFPPDP.deleteAll' farePolicyId
  QueriesFullFPMin.deleteAllByFarePolicyId (KTI.getId farePolicyId)
  deleteWithKV [Se.Is BeamFPPD.farePolicyId $ Se.Eq (KTI.getId farePolicyId)]

fromTTypeFarePolicyProgressiveDetails ::
  BeamFPPD.FarePolicyProgressiveDetails ->
  Maybe (NonEmpty Domain.FPProgressiveDetailsPerMinRateSection) ->
  NonEmpty QueriesFPPDP.FullFarePolicyProgressiveDetailsPerExtraKmRateSection ->
  FullFarePolicyProgressiveDetails
fromTTypeFarePolicyProgressiveDetails BeamFPPD.FarePolicyProgressiveDetailsT {..} fullMinFP fPPDP =
  ( KTI.Id farePolicyId,
    Domain.FPProgressiveDetails
      { baseDistance = baseDistance,
        baseFare = mkAmountWithDefault baseFareAmount baseFare,
        perExtraKmRateSections = snd <$> fPPDP,
        deadKmFare = mkAmountWithDefault deadKmFareAmount deadKmFare,
        pickupCharges = do
          let pChargesmin = fromMaybe deadKmFare pickupChargesMin
              pChargesmax = fromMaybe deadKmFare pickupChargesMax
          Domain.PickupCharges
            { pickupChargesMin = mkAmountWithDefault pickupChargesMinAmount pChargesmin,
              pickupChargesMax = mkAmountWithDefault pickupChargesMaxAmount pChargesmax
            },
        currency = fromMaybe INR currency,
        distanceUnit = fromMaybe Meter distanceUnit,
        perMinRateSections = fullMinFP,
        perMinRateDurationBasis = perMinRateDurationBasis,
        waitingChargeInfo =
          ((,) <$> waitingCharge <*> freeWatingTime) <&> \(waitingCharge', freeWaitingTime') ->
            Domain.WaitingChargeInfo
              { waitingCharge = waitingCharge',
                freeWaitingTime = freeWaitingTime'
              },
        nightShiftCharge = nightShiftCharge
      }
  )
