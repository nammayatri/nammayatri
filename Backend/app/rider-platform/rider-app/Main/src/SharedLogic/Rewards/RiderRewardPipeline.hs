{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Rewards.RiderRewardPipeline
  ( tryRiderRewardLogic,
  )
where

import qualified Data.Aeson as A
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.BookingStatus as DBookingStatus
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.RewardOffer as DReward
import qualified Domain.Types.Ride as DRide
import Kernel.Prelude
import Kernel.Storage.Clickhouse.Config (ClickhouseFlow)
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.BehaviorEngine.Orchestrator as BEOrch
import qualified Lib.BehaviorTracker.Snapshot as BTSnap
import qualified Lib.BehaviorTracker.Types as BTT
import Lib.Yudhishthira.Storage.Beam.BeamFlow (HasYudhishthiraTablesSchema)
import qualified Lib.Yudhishthira.Tools.DebugLog as LYDL
import qualified Lib.Yudhishthira.Types as LYT
import qualified SharedLogic.BehaviourManagement.RiderConsequenceDispatcher as RiderDispatch
import qualified Storage.CachedQueries.RewardOffer as QRewardOffer
import qualified Storage.Queries.Person as QPerson
import Tools.DynamicLogic (getAppDynamicLogic)
import Tools.Error

tryRiderRewardLogic ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    ClickhouseFlow m r,
    HasYudhishthiraTablesSchema,
    EncFlow m r
  ) =>
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DBookingStatus.BookingStatus ->
  DBooking.Booking ->
  DRide.Ride ->
  m ()
tryRiderRewardLogic riderId merchantId merchantOpCityId bookingStatus booking ride = do
  person <- QPerson.findById riderId >>= fromMaybeM (PersonDoesNotExist riderId.getId)
  let riderTagTexts = [raw | LYT.TagNameValueExpiry raw <- fromMaybe [] person.customerNammaTags]
  if null riderTagTexts
    then pure ()
    else do
      offers <- QRewardOffer.findAllActiveByMerchantOpCityId merchantOpCityId
      let matchingOffers =
            filter
              ( \o ->
                  o.entityType == DReward.RIDER
                    && o.triggerEvent == DReward.BookingComplete
                    && tagsMatch riderTagTexts o.requiredTags
              )
              offers
      if null matchingOffers
        then pure ()
        else do
          now <- getCurrentTime
          (logics, _) <- getAppDynamicLogic (cast merchantOpCityId) LYT.RIDER_REWARDS now Nothing Nothing
          if null logics
            then pure ()
            else do
              let actionEvent =
                    BTT.ActionEvent
                      { entityType = BTT.RIDER,
                        entityId = riderId.getId,
                        actionType = "BookingComplete",
                        merchantOperatingCityId = merchantOpCityId.getId,
                        flowContext = A.object ["matchingOfferIds" A..= map (getId . (.id)) matchingOffers],
                        eventData =
                          A.object
                            [ "bookingId" A..= booking.id.getId,
                              "rideId" A..= ride.id.getId,
                              "bookingStatus" A..= A.toJSON bookingStatus
                            ],
                        timestamp = now
                      }
                  entityState = A.object ["riderTags" A..= riderTagTexts]
              let counterConfig = BTT.CounterConfig {windowSizeDays = 365, counters = [], periods = []}
              snapshot <- BTSnap.buildSnapshotWithCooldowns counterConfig actionEvent entityState []
              let fetchRules dom = getAppDynamicLogic (cast merchantOpCityId) dom now Nothing Nothing
              output <- BEOrch.orchestrate snapshot LYDL.Rider (cast merchantOpCityId) LYT.RIDER_REWARDS fetchRules
              unless (null output.consequences && null output.communications) $ do
                let dispatchCtx =
                      RiderDispatch.RiderDispatchContext
                        { merchantId,
                          merchantOperatingCityId = merchantOpCityId,
                          bookingId = booking.id.getId,
                          rideId = Just ride.id.getId
                        }
                RiderDispatch.handleConsequences dispatchCtx riderId output.consequences
                RiderDispatch.handleCommunications riderId output.communications
                logInfo $ "Rider rewards applied via RIDER_REWARDS for rider " <> riderId.getId

tagsMatch :: [Text] -> [Text] -> Bool
tagsMatch riderTags requiredTags =
  null requiredTags || any (`elem` riderTags) requiredTags
