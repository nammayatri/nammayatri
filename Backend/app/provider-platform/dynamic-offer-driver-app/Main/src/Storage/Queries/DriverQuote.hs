{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.DriverQuote where

import Data.Int (Int32)
import qualified Domain.Types.DriverQuote as Domain
import Domain.Types.Person
import qualified Domain.Types.SearchRequest as DSReq
import qualified EulerHS.Extra.EulerDB as Extra
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common (addUTCTime, secondsToNominalDiffTime)
import qualified Lib.Mesh as Mesh
import qualified Sequelize as Se
import qualified Storage.Beam.DriverQuote as BeamDQ
import Storage.Queries.FareParameters as BeamQFP
-- import Storage.Queries.FullEntityBuilders (buildFullDriverQuote)
import Storage.Tabular.DriverQuote

-- import qualified Storage.Tabular.FareParameters.Instances as FareParamsT

create :: L.MonadFlow m => Domain.DriverQuote -> m (MeshResult ())
create dQuote = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' -> KV.createWoReturingKVConnector dbConf' Mesh.meshConfig (transformDomainDriverQuoteToBeam dQuote)
    Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

-- baseDriverQuoteQuery ::
--   From
--     ( SqlExpr (Entity DriverQuoteT)
--         :& SqlExpr (Entity Fare.FareParametersT)
--     )
-- baseDriverQuoteQuery =
--   table @DriverQuoteT
--     `innerJoin` table @Fare.FareParametersT
--       `Esq.on` ( \(rb :& farePars) ->
--                    rb ^. DriverQuoteFareParametersId ==. farePars ^. Fare.FareParametersTId
--                )

findById :: (L.MonadFlow m) => Id Domain.DriverQuote -> m (Maybe Domain.DriverQuote)
findById (Id driverQuoteId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbCOnf' -> do
      sR <- KV.findWithKVConnector dbCOnf' Mesh.meshConfig [Se.Is BeamDQ.id $ Se.Eq driverQuoteId]
      case sR of
        Left _ -> pure Nothing
        Right x -> mapM transformBeamDriverQuoteToDomain x
    Nothing -> pure Nothing

-- setInactiveByRequestId :: Id DSReq.SearchRequest -> SqlDB ()
-- setInactiveByRequestId searchReqId = Esq.update $ \p -> do
--   set p [DriverQuoteStatus =. val Domain.Inactive]
--   where_ $ p ^. DriverQuoteSearchRequestId ==. val (toKey searchReqId)

setInactiveByRequestId :: L.MonadFlow m => Id DSReq.SearchRequest -> m (MeshResult ())
setInactiveByRequestId (Id searchReqId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbCOnf' ->
      KV.updateWoReturningWithKVConnector
        dbCOnf'
        Mesh.meshConfig
        [Se.Set BeamDQ.status Domain.Inactive]
        [Se.Is BeamDQ.searchRequestId $ Se.Eq searchReqId]
    Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

findActiveQuotesByDriverId :: (L.MonadFlow m, MonadTime m) => Id Person -> Seconds -> m [Domain.DriverQuote]
findActiveQuotesByDriverId (Id driverId) driverUnlockDelay = do
  now <- getCurrentTime
  let delayToAvoidRaces = secondsToNominalDiffTime . negate $ driverUnlockDelay
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbCOnf' -> do
      srsz <- KV.findAllWithKVConnector dbCOnf' Mesh.meshConfig [Se.And [Se.Is BeamDQ.status $ Se.Eq Domain.Active, Se.Is BeamDQ.id $ Se.Eq driverId, Se.Is BeamDQ.validTill $ Se.GreaterThan (addUTCTime delayToAvoidRaces now)]]
      case srsz of
        Left _ -> pure []
        Right x -> mapM transformBeamDriverQuoteToDomain x
    Nothing -> pure []

findAllByRequestId :: L.MonadFlow m => Id DSReq.SearchRequest -> m [Domain.DriverQuote]
findAllByRequestId (Id searchReqId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbCOnf' -> do
      srsz <- KV.findAllWithKVConnector dbCOnf' Mesh.meshConfig [Se.And [Se.Is BeamDQ.status $ Se.Eq Domain.Active, Se.Is BeamDQ.id $ Se.Eq searchReqId]]
      case srsz of
        Left _ -> pure []
        Right x -> mapM transformBeamDriverQuoteToDomain x
    Nothing -> pure []

countAllByRequestId :: Transactionable m => Id DSReq.SearchRequest -> m Int32
countAllByRequestId searchReqId = do
  fmap (fromMaybe 0) $
    Esq.findOne $ do
      dQuote <- from $ table @DriverQuoteT
      where_ $
        dQuote ^. DriverQuoteStatus ==. val Domain.Active
          &&. dQuote ^. DriverQuoteSearchRequestId ==. val (toKey searchReqId)
      pure (countRows @Int32)

deleteByDriverId :: L.MonadFlow m => Id Person -> m ()
deleteByDriverId (Id driverId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' ->
      void $
        KV.deleteWithKVConnector
          dbConf'
          Mesh.meshConfig
          [Se.Is BeamDQ.driverId (Se.Eq driverId)]
    Nothing -> pure ()

-- findDriverQuoteBySearchId :: Transactionable m => Id DSReq.SearchRequest -> DTypeBuilder m (Maybe DriverQuoteT)
-- findDriverQuoteBySearchId searchReqId = Esq.findOne' $ do
--   driverQuote <- from $ table @DriverQuoteT
--   where_ $ driverQuote ^. DriverQuoteSearchRequestId ==. val (toKey searchReqId)
--   pure driverQuote

findDriverQuoteBySearchId :: (L.MonadFlow m) => Id DSReq.SearchRequest -> m (Maybe Domain.DriverQuote)
findDriverQuoteBySearchId (Id searchReqId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbCOnf' -> do
      sR <- KV.findWithKVConnector dbCOnf' Mesh.meshConfig [Se.Is BeamDQ.searchRequestId $ Se.Eq searchReqId]
      case sR of
        Left _ -> pure Nothing
        Right x -> mapM transformBeamDriverQuoteToDomain x
    Nothing -> pure Nothing

transformBeamDriverQuoteToDomain :: L.MonadFlow m => BeamDQ.DriverQuote -> m Domain.DriverQuote
transformBeamDriverQuoteToDomain BeamDQ.DriverQuoteT {..} = do
  fp <- BeamQFP.findById (Id fareParametersId)
  pure
    Domain.DriverQuote
      { id = Id id,
        transactionId = transactionId,
        searchRequestId = Id searchRequestId,
        searchRequestForDriverId = Id <$> searchRequestForDriverId,
        driverId = Id driverId,
        driverName = driverName,
        driverRating = driverRating,
        status = status,
        vehicleVariant = vehicleVariant,
        distance = distance,
        distanceToPickup = distanceToPickup,
        durationToPickup = durationToPickup,
        createdAt = createdAt,
        updatedAt = updatedAt,
        validTill = validTill,
        estimatedFare = estimatedFare,
        fareParams = fromJust fp, -- this should take a default value?
        providerId = Id providerId
      }

transformDomainDriverQuoteToBeam :: Domain.DriverQuote -> BeamDQ.DriverQuote
transformDomainDriverQuoteToBeam Domain.DriverQuote {..} =
  BeamDQ.DriverQuoteT
    { BeamDQ.id = getId id,
      BeamDQ.transactionId = transactionId,
      BeamDQ.searchRequestId = getId searchRequestId,
      BeamDQ.searchRequestForDriverId = getId <$> searchRequestForDriverId,
      BeamDQ.driverId = getId driverId,
      BeamDQ.driverName = driverName,
      BeamDQ.driverRating = driverRating,
      BeamDQ.status = status,
      BeamDQ.vehicleVariant = vehicleVariant,
      BeamDQ.distance = distance,
      BeamDQ.distanceToPickup = distanceToPickup,
      BeamDQ.durationToPickup = durationToPickup,
      BeamDQ.createdAt = createdAt,
      BeamDQ.updatedAt = updatedAt,
      BeamDQ.validTill = validTill,
      BeamDQ.estimatedFare = estimatedFare,
      BeamDQ.fareParametersId = getId fareParams.id,
      BeamDQ.providerId = getId providerId
    }
