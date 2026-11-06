{-
  Copyright 2022-23, Juspay India Pvt Ltd
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.FarePolicy.FarePolicyInterCityDetails where

import qualified Data.List.NonEmpty as NE
import qualified Domain.Types.FarePolicy as Domain
import qualified Domain.Types.FullFarePolicyInterCityDetailsPerKmRateSection as FullPerKmDomain
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id as KTI
import Kernel.Utils.Common
import Kernel.Utils.JSON (valueToMaybe)
import Sequelize as Se
import Storage.Beam.FarePolicy.FarePolicyInterCityDetails as BeamFPRD
import qualified Storage.Beam.FarePolicy.FarePolicyInterCityDetailsPricingSlabs as BeamFPICDPS
import qualified Storage.Queries.FarePolicy.FarePolicyInterCityDetailsPricingSlabs as QueriesFPICDPS
import qualified Storage.Queries.FullFarePolicyInterCityDetailsPerKmRateSection as QueriesFullFPICDPerKm

findById' :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => KTI.Id Domain.FarePolicy -> m (Maybe Domain.FullFarePolicyInterCityDetails)
findById' (KTI.Id farePolicyId') = findOneWithKV [Se.Is BeamFPRD.farePolicyId $ Se.Eq farePolicyId']

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Domain.FullFarePolicyInterCityDetails -> m ()
create farePolicyInterCityDetails = do
  let fpId = KTI.getId $ fst farePolicyInterCityDetails
      details = snd farePolicyInterCityDetails
  mapM_ QueriesFPICDPS.create (map (fst farePolicyInterCityDetails,) (NE.toList details.pricingSlabs))
  now <- getCurrentTime
  QueriesFullFPICDPerKm.createMany $
    mkFullPerKmRateSections fpId False now details.perKmRateSectionsOneWay
      <> mkFullPerKmRateSections fpId True now details.perKmRateSectionsRoundTrip
  createWithKV farePolicyInterCityDetails
  where
    mkFullPerKmRateSections :: Text -> Bool -> UTCTime -> Maybe (NonEmpty Domain.FPInterCityDetailsPerKmRateSection) -> [FullPerKmDomain.FullFarePolicyInterCityDetailsPerKmRateSection]
    mkFullPerKmRateSections fpIdText isRoundTrip now =
      maybe [] $
        map
          ( \s ->
              FullPerKmDomain.FullFarePolicyInterCityDetailsPerKmRateSection
                { farePolicyId = fpIdText,
                  isRoundTrip,
                  startDistance = s.startDistance,
                  perKmRate = s.perKmRate,
                  createdAt = now,
                  updatedAt = now
                }
          )
          . NE.toList

delete :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => KTI.Id Domain.FarePolicy -> m ()
delete farePolicyId = do
  QueriesFPICDPS.delete farePolicyId
  QueriesFullFPICDPerKm.deleteAllByFarePolicyId (KTI.getId farePolicyId)
  deleteWithKV [Se.Is BeamFPRD.farePolicyId $ Se.Eq (KTI.getId farePolicyId)]

instance FromTType' BeamFPRD.FarePolicyInterCityDetails Domain.FullFarePolicyInterCityDetails where
  fromTType' farePolicyInterCityDetails = do
    fullFPICDPS <- QueriesFPICDPS.findAll' (KTI.Id farePolicyInterCityDetails.farePolicyId)
    fPICDPS <- fromMaybeM (InternalError "No pricing slab found for intercity") (NE.nonEmpty fullFPICDPS) -- check it
    perKmRateSections <- QueriesFullFPICDPerKm.findAllByFarePolicyId farePolicyInterCityDetails.farePolicyId
    Just <$> fromTTypeFarePolicyInterCityDetails farePolicyInterCityDetails fPICDPS perKmRateSections

fromTTypeFarePolicyInterCityDetails ::
  MonadFlow m =>
  BeamFPRD.FarePolicyInterCityDetails ->
  NonEmpty BeamFPICDPS.FullFarePolicyInterCityDetailsPricingSlabs ->
  [FullPerKmDomain.FullFarePolicyInterCityDetailsPerKmRateSection] ->
  m Domain.FullFarePolicyInterCityDetails
fromTTypeFarePolicyInterCityDetails BeamFPRD.FarePolicyInterCityDetailsT {..} fPICDPS perKmRateSections = do
  let permitChargesByState = valueToMaybe =<< stateEntryPermitChargesByState
      (oneWayPerKmRateSections, roundTripPerKmRateSections) = Domain.mkPerKmRateSections perKmRateSections
  when (isJust stateEntryPermitChargesByState && isNothing permitChargesByState) $
    logError $ "Invalid state_entry_permit_charges_by_state, no permit will be charged for farePolicyId: " <> farePolicyId
  pure
    ( KTI.Id farePolicyId,
      Domain.FPInterCityDetails
        { pricingSlabs = snd <$> fPICDPS,
          perKmRateSectionsOneWay = oneWayPerKmRateSections,
          perKmRateSectionsRoundTrip = roundTripPerKmRateSections,
          waitingChargeInfo =
            ((,) <$> waitingCharge <*> freeWatingTime) <&> \(waitingCharge', freeWaitingTime') ->
              Domain.WaitingChargeInfo
                { waitingCharge = waitingCharge',
                  freeWaitingTime = freeWaitingTime'
                },
          stateEntryPermitCharges = Nothing, -- column kept for older pods, search sets this per trip
          stateEntryPermitChargesByState = permitChargesByState,
          ..
        }
    )

instance ToTType' BeamFPRD.FarePolicyInterCityDetails Domain.FullFarePolicyInterCityDetails where
  toTType' (KTI.Id farePolicyId, Domain.FPInterCityDetails {..}) =
    BeamFPRD.FarePolicyInterCityDetailsT
      { farePolicyId = farePolicyId,
        freeWatingTime = (.freeWaitingTime) <$> waitingChargeInfo,
        waitingCharge = (.waitingCharge) <$> waitingChargeInfo,
        stateEntryPermitCharges = Nothing,
        stateEntryPermitChargesByState = toJSON <$> stateEntryPermitChargesByState,
        ..
      }
