{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Estimate where

import Data.Tuple.Extra
import Domain.Types.Estimate as DE
import Domain.Types.SearchRequest
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id (Id (Id, getId))
import Lib.Utils
import qualified Sequelize as Se
import qualified Storage.Beam.Estimate as BeamE
import qualified Storage.Queries.EstimateBreakup as QEB
import Storage.Queries.FullEntityBuilders (buildFullEstimate)
import qualified Storage.Queries.TripTerms as QTT
import Storage.Tabular.Estimate
import Storage.Tabular.TripTerms

-- order of creating entites make sense!
create :: Estimate -> SqlDB ()
create estimate =
  Esq.withFullEntity estimate $ \(estimateT, estimateBreakupT, mbTripTermsT) -> do
    traverse_ Esq.create' mbTripTermsT
    Esq.create' estimateT
    traverse_ Esq.create' estimateBreakupT

createMany :: [Estimate] -> SqlDB ()
createMany estimates =
  Esq.withFullEntities estimates $ \list -> do
    let estimateTs = map fst3 list
        estimateBreakupT = map snd3 list
        tripTermsTs = mapMaybe thd3 list
    Esq.createMany' tripTermsTs
    Esq.createMany' estimateTs
    traverse_ Esq.createMany' estimateBreakupT

fullEstimateTable ::
  From
    ( Table EstimateT
        :& Esq.MbTable TripTermsT
    )
fullEstimateTable =
  table @EstimateT
    `leftJoin` table @TripTermsT
      `Esq.on` ( \(estimate :& mbTripTerms) ->
                   estimate ^. EstimateTripTermsId ==. mbTripTerms ?. TripTermsTId
               )

findById :: Transactionable m => Id Estimate -> m (Maybe Estimate)
findById estimateId = Esq.buildDType $ do
  mbFullEstimateT <- Esq.findOne' $ do
    (estimate :& mbTripTerms) <- from fullEstimateTable
    where_ $ estimate ^. EstimateTId ==. val (toKey estimateId)
    pure (estimate, mbTripTerms)
  mapM buildFullEstimate mbFullEstimateT

findById' :: L.MonadFlow m => Id Estimate -> m (Maybe Estimate)
findById' (Id estimateId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamE.EstimateT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> do
      res <- KV.findWithKVConnector dbCOnf' updatedMeshConfig [Se.Is BeamE.id $ Se.Eq estimateId]
      case res of
        Right (Just x) -> transformBeamEstimateToDomain x
        _ -> pure Nothing
    Nothing -> pure Nothing

findAllBySRId :: Transactionable m => Id SearchRequest -> m [Estimate]
findAllBySRId searchRequestId = Esq.buildDType $ do
  fullEstimateTs <- Esq.findAll' $ do
    (estimate :& mbTripTerms) <- from fullEstimateTable
    where_ $ estimate ^. EstimateRequestId ==. val (toKey searchRequestId)
    pure (estimate, mbTripTerms)
  mapM buildFullEstimate fullEstimateTs

findAllBySRId' :: L.MonadFlow m => Id SearchRequest -> m [Estimate]
findAllBySRId' (Id searchRequestId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamE.EstimateT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> do
      res <- KV.findAllWithKVConnector dbCOnf' updatedMeshConfig [Se.Is BeamE.requestId $ Se.Eq searchRequestId]
      case res of
        Right x -> catMaybes <$> traverse transformBeamEstimateToDomain x
        _ -> pure []
    Nothing -> pure []

findByBPPEstimateId :: Transactionable m => Id BPPEstimate -> m (Maybe Estimate)
findByBPPEstimateId bppEstimateId_ = Esq.buildDType $ do
  mbFullEstimateT <- Esq.findOne' $ do
    (estimate :& mbTripTerms) <- from fullEstimateTable
    where_ $ estimate ^. EstimateBppEstimateId ==. val (getId bppEstimateId_)
    pure (estimate, mbTripTerms)
  mapM buildFullEstimate mbFullEstimateT

findByBPPEstimateId' :: L.MonadFlow m => Id BPPEstimate -> m (Maybe Estimate)
findByBPPEstimateId' (Id bppEstimateId_) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamE.EstimateT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> do
      res <- KV.findWithKVConnector dbCOnf' updatedMeshConfig [Se.Is BeamE.bppEstimateId $ Se.Eq bppEstimateId_]
      case res of
        Right (Just x) -> transformBeamEstimateToDomain x
        _ -> pure Nothing
    Nothing -> pure Nothing

updateStatus ::
  Id Estimate ->
  EstimateStatus ->
  SqlDB ()
updateStatus estimateId status_ = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ EstimateUpdatedAt =. val now,
        EstimateStatus =. val status_
      ]
    where_ $ tbl ^. EstimateId ==. val (getId estimateId)

updateStatusByBppEstimateId' :: (L.MonadFlow m, MonadTime m) => Id BPPEstimate -> EstimateStatus -> m (MeshResult ())
updateStatusByBppEstimateId' (Id bppEstimateId_) status_ = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamE.EstimateT
  let updatedMeshConfig = setMeshConfig modelName
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        updatedMeshConfig
        [ Se.Set BeamE.updatedAt now,
          Se.Set BeamE.status status_
        ]
        [Se.Is BeamE.bppEstimateId (Se.Eq bppEstimateId_)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

getStatus ::
  (Transactionable m) =>
  Id Estimate ->
  m (Maybe EstimateStatus)
getStatus estimateId = do
  findOne $ do
    estimateT <- from $ table @EstimateT
    where_ $
      estimateT ^. EstimateId ==. val (getId estimateId)
    return $ estimateT ^. EstimateStatus

getStatus' :: L.MonadFlow m => Id Estimate -> m (Maybe EstimateStatus)
getStatus' (Id estimateId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamE.EstimateT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> do
      res <- KV.findWithKVConnector dbCOnf' updatedMeshConfig [Se.Is BeamE.id $ Se.Eq estimateId]
      case res of
        Right (Just x) -> do
          eStatus <- transformBeamEstimateToDomain x
          pure $ DE.status <$> eStatus
        _ -> pure Nothing
    Nothing -> pure Nothing

updateStatusByRequestId ::
  Id SearchRequest ->
  EstimateStatus ->
  SqlDB ()
updateStatusByRequestId searchId status_ = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ EstimateUpdatedAt =. val now,
        EstimateStatus =. val status_
      ]
    where_ $ tbl ^. EstimateRequestId ==. val (toKey searchId)

updateStatusByRequestId' :: (L.MonadFlow m, MonadTime m) => Id SearchRequest -> EstimateStatus -> m (MeshResult ())
updateStatusByRequestId' (Id searchId) status_ = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamE.EstimateT
  let updatedMeshConfig = setMeshConfig modelName
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        updatedMeshConfig
        [ Se.Set BeamE.updatedAt now,
          Se.Set BeamE.status status_
        ]
        [Se.Is BeamE.requestId (Se.Eq searchId)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

transformBeamEstimateToDomain :: L.MonadFlow m => BeamE.Estimate -> m (Maybe Estimate)
transformBeamEstimateToDomain e@BeamE.EstimateT {..} = do
  etB <- QEB.findAllByEstimateId' (Id e.bppEstimateId)
  trip <- if isJust e.tripTermsId then QTT.findById'' (Id (fromJust e.tripTermsId)) else pure Nothing
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

transformDomainEstimateToBeam :: Estimate -> BeamE.Estimate
transformDomainEstimateToBeam Estimate {..} =
  BeamE.defaultEstimate
    { BeamE.id = getId id,
      BeamE.requestId = getId requestId,
      BeamE.merchantId = getId <$> merchantId,
      BeamE.bppEstimateId = getId bppEstimateId,
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
