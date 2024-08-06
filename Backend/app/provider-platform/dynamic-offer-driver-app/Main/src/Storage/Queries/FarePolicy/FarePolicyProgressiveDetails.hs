{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.FarePolicy.FarePolicyProgressiveDetails where

import qualified Data.List.NonEmpty as NE
import qualified Domain.Types.FarePolicy as Domain
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id as KTI
import Kernel.Utils.Common
import Sequelize as Se
import Storage.Beam.FarePolicy.FarePolicyProgressiveDetails as BeamFPPD
import qualified Storage.Beam.FarePolicy.FarePolicyProgressiveDetails.FarePolicyProgressiveDetailsPerExtraKmRateSection as BeamFPPDP
import qualified Storage.Queries.FarePolicy.FarePolicyProgressiveDetails.FarePolicyProgressiveDetailsPerExtraKmRateSection as QueriesFPPDP
import qualified Storage.Queries.FarePolicy.FarePolicyProgressiveDetails.FarePolicyProgressiveDetailsPerMinRateSection as QFPPDPerMinute

findById' :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => KTI.Id Domain.FarePolicy -> m (Maybe Domain.FullFarePolicyProgressiveDetails)
findById' (KTI.Id farePolicyId') = findOneWithKV [Se.Is BeamFPPD.farePolicyId $ Se.Eq farePolicyId']

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Domain.FullFarePolicyProgressiveDetails -> m ()
create farePolicyProgressiveDetails = do
  mapM_ (QueriesFPPDP.create . (fst farePolicyProgressiveDetails,)) (NE.toList (snd farePolicyProgressiveDetails).perExtraKmRateSections)
  let mbPerMinRateSections = snd farePolicyProgressiveDetails & (.perMinRateSections)
  whenJust mbPerMinRateSections $ mapM_ (QFPPDPerMinute.create . (KTI.getId $ fst farePolicyProgressiveDetails,)) . NE.toList
  createWithKV farePolicyProgressiveDetails

delete :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => KTI.Id Domain.FarePolicy -> m ()
delete farePolicyId = do
  QueriesFPPDP.deleteAll' farePolicyId
  QFPPDPerMinute.deleteAll' farePolicyId.getId
  deleteWithKV [Se.Is BeamFPPD.farePolicyId $ Se.Eq (KTI.getId farePolicyId)]

instance FromTType' BeamFPPD.FarePolicyProgressiveDetails Domain.FullFarePolicyProgressiveDetails where
  fromTType' farePolicyProgressiveDetails = do
    fullFPPDP <- QueriesFPPDP.findAll' (KTI.Id farePolicyProgressiveDetails.farePolicyId)
    fullMinFP <- NE.nonEmpty <$> QFPPDPerMinute.findAll (KTI.Id farePolicyProgressiveDetails.farePolicyId)
    fPPDP <- fromMaybeM (InternalError "FromLocation not found") (NE.nonEmpty fullFPPDP)
    pure . Just $ fromTTypeFarePolicyProgressiveDetails farePolicyProgressiveDetails fullMinFP fPPDP

fromTTypeFarePolicyProgressiveDetails ::
  BeamFPPD.FarePolicyProgressiveDetails ->
  Maybe (NonEmpty Domain.FPProgressiveDetailsPerMinRateSection) ->
  NonEmpty BeamFPPDP.FullFarePolicyProgressiveDetailsPerExtraKmRateSection ->
  Domain.FullFarePolicyProgressiveDetails
fromTTypeFarePolicyProgressiveDetails BeamFPPD.FarePolicyProgressiveDetailsT {..} fullMinFP fPPDP =
  ( KTI.Id farePolicyId,
    Domain.FPProgressiveDetails
      { baseDistance = baseDistance,
        baseFare = mkAmountWithDefault baseFareAmount baseFare,
        perExtraKmRateSections = snd <$> fPPDP,
        deadKmFare = mkAmountWithDefault deadKmFareAmount deadKmFare,
        currency = fromMaybe INR currency,
        distanceUnit = fromMaybe Meter distanceUnit,
        perMinRateSections = fullMinFP,
        waitingChargeInfo =
          ((,) <$> waitingCharge <*> freeWatingTime) <&> \(waitingCharge', freeWaitingTime') ->
            Domain.WaitingChargeInfo
              { waitingCharge = waitingCharge',
                freeWaitingTime = freeWaitingTime'
              },
        nightShiftCharge = nightShiftCharge
      }
  )

instance ToTType' BeamFPPD.FarePolicyProgressiveDetails Domain.FullFarePolicyProgressiveDetails where
  toTType' (KTI.Id farePolicyId, Domain.FPProgressiveDetails {..}) =
    BeamFPPD.FarePolicyProgressiveDetailsT
      { farePolicyId = farePolicyId,
        baseDistance = baseDistance,
        baseFare = roundToIntegral baseFare,
        baseFareAmount = Just baseFare,
        freeWatingTime = (.freeWaitingTime) <$> waitingChargeInfo,
        deadKmFare = roundToIntegral deadKmFare,
        deadKmFareAmount = Just deadKmFare,
        currency = Just currency,
        distanceUnit = Just distanceUnit,
        waitingCharge = (.waitingCharge) <$> waitingChargeInfo,
        nightShiftCharge = nightShiftCharge
      }
