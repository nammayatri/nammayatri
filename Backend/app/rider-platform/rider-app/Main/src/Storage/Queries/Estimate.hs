{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Estimate where

-- import Data.Tuple.Extra
import Domain.Types.Estimate as DE
import Domain.Types.SearchRequest
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id (Id (Id, getId))
import Lib.Utils
import qualified Sequelize as Se
import qualified Storage.Beam.Estimate as BeamE
import qualified Storage.Queries.EstimateBreakup as QEB
-- import Storage.Queries.FullEntityBuilders (buildFullEstimate)
import qualified Storage.Queries.TripTerms as QTT

-- import qualified Kernel.Beam.Types as KBT
-- import qualified Sequelize as Se
-- import EulerHS.KVConnector.Types
-- import Lib.Utils (setMeshConfig)

createEstimate :: (L.MonadFlow m, Log m) => Estimate -> m ()
createEstimate = createWithKV

create :: (L.MonadFlow m, Log m) => Estimate -> m ()
create estimate = do
  _ <- traverse_ QTT.createTripTerms estimate.tripTerms
  _ <- createEstimate estimate
  traverse_ QEB.create estimate.estimateBreakupList

-- -- order of creating entites make sense!
-- create :: Estimate -> SqlDB ()
-- create estimate =
--   Esq.withFullEntity estimate $ \(estimateT, estimateBreakupT, mbTripTermsT) -> do
--     traverse_ Esq.create' mbTripTermsT
--     Esq.create' estimateT
--     traverse_ Esq.create' estimateBreakupT

-- createMany :: [Estimate] -> SqlDB ()
-- createMany estimates =
--   Esq.withFullEntities estimates $ \list -> do
--     let estimateTs = map fst3 list
--         estimateBreakupT = map snd3 list
--         tripTermsTs = mapMaybe thd3 list
--     Esq.createMany' tripTermsTs
--     Esq.createMany' estimateTs
--     traverse_ Esq.createMany' estimateBreakupT

createMany :: (L.MonadFlow m, Log m) => [Estimate] -> m ()
createMany = traverse_ create

-- fullEstimateTable ::
--   From
--     ( Table EstimateT
--         :& Esq.MbTable TripTermsT
--     )
-- fullEstimateTable =
--   table @EstimateT
--     `leftJoin` table @TripTermsT
--       `Esq.on` ( \(estimate :& mbTripTerms) ->
--                    estimate ^. EstimateTripTermsId ==. mbTripTerms ?. TripTermsTId
--                )

-- findById :: Transactionable m => Id Estimate -> m (Maybe Estimate)
-- findById estimateId = Esq.buildDType $ do
--   mbFullEstimateT <- Esq.findOne' $ do
--     (estimate :& mbTripTerms) <- from fullEstimateTable
--     where_ $ estimate ^. EstimateTId ==. val (toKey estimateId)
--     pure (estimate, mbTripTerms)
--   mapM buildFullEstimate mbFullEstimateT

findById :: (L.MonadFlow m, Log m) => Id Estimate -> m (Maybe Estimate)
findById (Id estimateId) = findOneWithKV [Se.Is BeamE.id $ Se.Eq estimateId]

findByIdInReplica :: (L.MonadFlow m, Log m) => Id Estimate -> m (Maybe Estimate)
findByIdInReplica (Id estimateId) = findOneWithKvInReplica [Se.Is BeamE.id $ Se.Eq estimateId]

-- findAllBySRId :: Transactionable m => Id SearchRequest -> m [Estimate]
-- findAllBySRId searchRequestId = Esq.buildDType $ do
--   fullEstimateTs <- Esq.findAll' $ do
--     (estimate :& mbTripTerms) <- from fullEstimateTable
--     where_ $ estimate ^. EstimateRequestId ==. val (toKey searchRequestId)
--     pure (estimate, mbTripTerms)
--   mapM buildFullEstimate fullEstimateTs

findAllBySRId :: (L.MonadFlow m, Log m) => Id SearchRequest -> m [Estimate]
findAllBySRId (Id searchRequestId) = findAllWithKV [Se.Is BeamE.requestId $ Se.Eq searchRequestId]

findAllBySRIdInReplica :: (L.MonadFlow m, Log m) => Id SearchRequest -> m [Estimate]
findAllBySRIdInReplica (Id searchRequestId) = findAllWithKvInReplica [Se.Is BeamE.requestId $ Se.Eq searchRequestId]

-- findByBPPEstimateId :: Transactionable m => Id BPPEstimate -> m (Maybe Estimate)
-- findByBPPEstimateId bppEstimateId_ = Esq.buildDType $ do
--   mbFullEstimateT <- Esq.findOne' $ do
--     (estimate :& mbTripTerms) <- from fullEstimateTable
--     where_ $ estimate ^. EstimateBppEstimateId ==. val (getId bppEstimateId_)
--     pure (estimate, mbTripTerms)
--   mapM buildFullEstimate mbFullEstimateT

findByBPPEstimateId :: (L.MonadFlow m, Log m) => Id BPPEstimate -> m (Maybe Estimate)
findByBPPEstimateId (Id bppEstimateId_) = findOneWithKV [Se.Is BeamE.bppEstimateId $ Se.Eq bppEstimateId_]

-- updateStatus ::
--   Id Estimate ->
--   EstimateStatus ->
--   SqlDB ()
-- updateStatus estimateId status_ = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ EstimateUpdatedAt =. val now,
--         EstimateStatus =. val status_
--       ]
--     where_ $ tbl ^. EstimateId ==. val (getId estimateId)

updateStatus :: (L.MonadFlow m, MonadTime m, Log m) => Id Estimate -> EstimateStatus -> m ()
updateStatus (Id estimateId) status_ = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamE.updatedAt now,
      Se.Set BeamE.status status_
    ]
    [Se.Is BeamE.id (Se.Eq estimateId)]

-- getStatus ::
--   (Transactionable m) =>
--   Id Estimate ->
--   m (Maybe EstimateStatus)
-- getStatus estimateId = do
--   findOne $ do
--     estimateT <- from $ table @EstimateT
--     where_ $
--       estimateT ^. EstimateId ==. val (getId estimateId)
--     return $ estimateT ^. EstimateStatus

getStatus :: (L.MonadFlow m, Log m) => Id Estimate -> m (Maybe EstimateStatus)
getStatus (Id estimateId) = findOneWithKV [Se.Is BeamE.id $ Se.Eq estimateId] <&> (DE.status <$>)

-- updateStatusByRequestId ::
--   Id SearchRequest ->
--   EstimateStatus ->
--   SqlDB ()
-- updateStatusByRequestId searchId status_ = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ EstimateUpdatedAt =. val now,
--         EstimateStatus =. val status_
--       ]
--     where_ $ tbl ^. EstimateRequestId ==. val (toKey searchId)

updateStatusByRequestId :: (L.MonadFlow m, MonadTime m, Log m) => Id SearchRequest -> EstimateStatus -> m ()
updateStatusByRequestId (Id searchId) status_ = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamE.updatedAt now,
      Se.Set BeamE.status status_
    ]
    [Se.Is BeamE.requestId (Se.Eq searchId)]

instance FromTType' BeamE.Estimate Estimate where
  fromTType' BeamE.EstimateT {..} = do
    etB <- QEB.findAllByEstimateIdT (Id bppEstimateId)
    trip <- if isJust tripTermsId then QTT.findById'' (Id (fromJust tripTermsId)) else pure Nothing
    pUrl <- parseBaseUrl providerUrl
    let totalFareRange =
          DE.FareRange
            { minFare = roundToIntegral minTotalFare,
              maxFare = roundToIntegral maxTotalFare
            }
    pure $
      Just $
        Estimate
          { id = Id id,
            requestId = Id requestId,
            merchantId = Id <$> merchantId,
            bppEstimateId = Id bppEstimateId,
            itemId = itemId,
            discount = roundToIntegral <$> discount,
            estimatedFare = roundToIntegral estimatedFare,
            estimatedTotalFare = roundToIntegral estimatedTotalFare,
            totalFareRange = totalFareRange,
            estimatedDuration = estimatedDuration,
            estimatedDistance = estimatedDistance,
            device = device,
            providerId = providerId,
            providerUrl = pUrl,
            providerName = providerName,
            providerMobileNumber = providerMobileNumber,
            providerCompletedRidesCount = providerCompletedRidesCount,
            vehicleVariant = vehicleVariant,
            tripTerms = trip,
            estimateBreakupList = etB,
            nightShiftInfo =
              ((,,,) <$> nightShiftCharge <*> oldNightShiftCharge <*> nightShiftStart <*> nightShiftEnd)
                <&> \(nightShiftCharge', oldNightShiftCharge', nightShiftStart', nightShiftEnd') ->
                  DE.NightShiftInfo
                    { nightShiftCharge = nightShiftCharge',
                      oldNightShiftCharge = oldNightShiftCharge',
                      nightShiftStart = nightShiftStart',
                      nightShiftEnd = nightShiftEnd'
                    },
            status = status,
            waitingCharges = DE.WaitingCharges waitingChargePerMin,
            driversLocation = driversLocation,
            specialLocationTag = specialLocationTag,
            updatedAt = updatedAt,
            createdAt = createdAt
          }

instance ToTType' BeamE.Estimate Estimate where
  toTType' Estimate {..} = do
    BeamE.EstimateT
      { BeamE.id = getId id,
        BeamE.requestId = getId requestId,
        BeamE.merchantId = getId <$> merchantId,
        BeamE.bppEstimateId = getId bppEstimateId,
        BeamE.itemId = itemId,
        BeamE.estimatedFare = realToFrac estimatedFare,
        BeamE.discount = realToFrac <$> discount,
        BeamE.estimatedTotalFare = realToFrac estimatedTotalFare,
        BeamE.minTotalFare = realToFrac totalFareRange.minFare,
        BeamE.maxTotalFare = realToFrac totalFareRange.maxFare,
        BeamE.estimatedDuration = estimatedDuration,
        BeamE.estimatedDistance = estimatedDistance,
        BeamE.device = device,
        BeamE.providerId = providerId,
        BeamE.providerUrl = showBaseUrl providerUrl,
        BeamE.providerName = providerName,
        BeamE.providerMobileNumber = providerMobileNumber,
        BeamE.providerCompletedRidesCount = providerCompletedRidesCount,
        BeamE.vehicleVariant = vehicleVariant,
        BeamE.driversLocation = driversLocation,
        BeamE.tripTermsId = getId <$> (tripTerms <&> (.id)),
        BeamE.nightShiftCharge = nightShiftInfo <&> (.nightShiftCharge),
        BeamE.oldNightShiftCharge = nightShiftInfo <&> (.oldNightShiftCharge),
        BeamE.nightShiftStart = nightShiftInfo <&> (.nightShiftStart),
        BeamE.nightShiftEnd = nightShiftInfo <&> (.nightShiftEnd),
        BeamE.status = status,
        BeamE.waitingChargePerMin = waitingCharges.waitingChargePerMin,
        BeamE.specialLocationTag = specialLocationTag,
        BeamE.updatedAt = updatedAt,
        BeamE.createdAt = createdAt
      }
