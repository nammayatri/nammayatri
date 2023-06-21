{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.DriverQuote where

import qualified Data.Time as T
import qualified Domain.Types.DriverQuote as Domain
import Domain.Types.Person
import qualified Domain.Types.SearchTry as DST
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common (addUTCTime, secondsToNominalDiffTime)
import Lib.Utils (setMeshConfig)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverQuote as BeamDQ
import Storage.Queries.FareParameters as BeamQFP
import qualified Storage.Queries.FareParameters as SQFP
import Storage.Tabular.DriverQuote
import qualified Storage.Tabular.FareParameters as Fare

-- TODO @Vijay Gupta - update the following function Done
-- create :: Domain.DriverQuote -> SqlDB ()
-- create dQuote = Esq.runTransaction $
--   withFullEntity dQuote $ \(dQuoteT, (fareParams', fareParamsDetais)) -> do
--     Esq.create' fareParams'
--     case fareParamsDetais of
--       FareParamsT.ProgressiveDetailsT fppdt -> Esq.create' fppdt
--       FareParamsT.SlabDetailsT -> return ()
--     Esq.create' dQuoteT

create :: L.MonadFlow m => Domain.DriverQuote -> m (MeshResult ())
create dQuote = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDQ.DriverQuoteT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> do
      SQFP.create dQuote.fareParams
      KV.createWoReturingKVConnector dbConf' updatedMeshConfig (transformDomainDriverQuoteToBeam dQuote)
    Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

baseDriverQuoteQuery ::
  From
    ( SqlExpr (Entity DriverQuoteT)
        :& SqlExpr (Entity Fare.FareParametersT)
    )
baseDriverQuoteQuery =
  table @DriverQuoteT
    `innerJoin` table @Fare.FareParametersT
      `Esq.on` ( \(rb :& farePars) ->
                   rb ^. DriverQuoteFareParametersId ==. farePars ^. Fare.FareParametersTId
               )

findById :: (L.MonadFlow m) => Id Domain.DriverQuote -> m (Maybe Domain.DriverQuote)
findById (Id driverQuoteId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDQ.DriverQuoteT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> do
      driverQuote <- KV.findWithKVConnector dbCOnf' updatedMeshConfig [Se.Is BeamDQ.id $ Se.Eq driverQuoteId]
      case driverQuote of
        Right (Just driverQuote') -> transformBeamDriverQuoteToDomain driverQuote'
        _ -> pure Nothing
    Nothing -> pure Nothing

setInactiveBySTId :: L.MonadFlow m => Id DST.SearchTry -> m (MeshResult ())
setInactiveBySTId (Id searchTryId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDQ.DriverQuoteT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbCOnf' ->
      KV.updateWoReturningWithKVConnector
        dbCOnf'
        updatedMeshConfig
        [Se.Set BeamDQ.status Domain.Inactive]
        [Se.Is BeamDQ.searchTryId $ Se.Eq searchTryId]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

findActiveQuotesByDriverId :: (L.MonadFlow m, MonadTime m) => Id Person -> Seconds -> m [Domain.DriverQuote]
findActiveQuotesByDriverId (Id driverId) driverUnlockDelay = do
  now <- getCurrentTime
  let delayToAvoidRaces = secondsToNominalDiffTime . negate $ driverUnlockDelay
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDQ.DriverQuoteT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> do
      dQuote <- KV.findAllWithKVConnector dbConf' updatedMeshConfig [Se.And [Se.Is BeamDQ.status $ Se.Eq Domain.Active, Se.Is BeamDQ.id $ Se.Eq driverId, Se.Is BeamDQ.validTill $ Se.GreaterThan (T.utcToLocalTime (T.TimeZone (5 * 60 + 30) False "IST") $ addUTCTime delayToAvoidRaces now)]]
      case dQuote of
        Right dQuote' -> catMaybes <$> traverse transformBeamDriverQuoteToDomain dQuote'
        _ -> pure []
    _ -> pure []

findDriverQuoteBySTId :: L.MonadFlow m => Id DST.SearchTry -> m (Maybe Domain.DriverQuote)
findDriverQuoteBySTId (Id searchTryId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDQ.DriverQuoteT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> do
      driverQuote <- KV.findWithKVConnector dbCOnf' updatedMeshConfig [Se.Is BeamDQ.searchTryId $ Se.Eq searchTryId]
      case driverQuote of
        Right (Just driverQuote') -> transformBeamDriverQuoteToDomain driverQuote'
        _ -> pure Nothing
    Nothing -> pure Nothing

deleteByDriverId :: L.MonadFlow m => Id Person -> m ()
deleteByDriverId (Id driverId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDQ.DriverQuoteT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' ->
      void $
        KV.deleteWithKVConnector
          dbConf'
          updatedMeshConfig
          [Se.Is BeamDQ.driverId (Se.Eq driverId)]
    Nothing -> pure ()

findAllBySTId :: L.MonadFlow m => Id DST.SearchTry -> m [Domain.DriverQuote]
findAllBySTId (Id searchTryId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDQ.DriverQuoteT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> do
      do
        res <-
          KV.findAllWithKVConnector
            dbConf'
            updatedMeshConfig
            [ Se.And [Se.Is BeamDQ.searchTryId $ Se.Eq searchTryId, Se.Is BeamDQ.status $ Se.Eq Domain.Active]
            ]
        case res of
          Right res' -> catMaybes <$> traverse transformBeamDriverQuoteToDomain res'
          _ -> pure []
    Nothing -> pure []

countAllBySTId :: L.MonadFlow m => Id DST.SearchTry -> m Int
countAllBySTId searchTId = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDQ.DriverQuoteT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> do
      res <- KV.findAllWithKVConnector dbConf' updatedMeshConfig [Se.And [Se.Is BeamDQ.searchTryId $ Se.Eq (getId searchTId)]]
      case res of
        Right res' -> pure $ length res'
        _ -> pure 0
    Nothing -> pure 0

transformBeamDriverQuoteToDomain :: L.MonadFlow m => BeamDQ.DriverQuote -> m (Maybe Domain.DriverQuote)
transformBeamDriverQuoteToDomain BeamDQ.DriverQuoteT {..} = do
  fp <- BeamQFP.findById (Id fareParametersId)
  if isJust fp
    then
      pure $
        Just
          Domain.DriverQuote
            { id = Id id,
              requestId = Id requestId,
              searchTryId = Id searchTryId,
              searchRequestForDriverId = Id <$> searchRequestForDriverId,
              driverId = Id driverId,
              driverName = driverName,
              driverRating = driverRating,
              status = status,
              vehicleVariant = vehicleVariant,
              distance = distance,
              distanceToPickup = distanceToPickup,
              durationToPickup = durationToPickup,
              createdAt = T.localTimeToUTC T.utc createdAt,
              updatedAt = T.localTimeToUTC T.utc updatedAt,
              validTill = T.localTimeToUTC T.utc validTill,
              estimatedFare = estimatedFare,
              fareParams = fromJust fp, -- this should take a default value?
              providerId = Id providerId,
              specialLocationTag = specialLocationTag
            }
    else pure Nothing

transformDomainDriverQuoteToBeam :: Domain.DriverQuote -> BeamDQ.DriverQuote
transformDomainDriverQuoteToBeam Domain.DriverQuote {..} =
  BeamDQ.DriverQuoteT
    { BeamDQ.id = getId id,
      BeamDQ.requestId = getId requestId,
      BeamDQ.searchTryId = getId searchTryId,
      BeamDQ.searchRequestForDriverId = getId <$> searchRequestForDriverId,
      BeamDQ.driverId = getId driverId,
      BeamDQ.driverName = driverName,
      BeamDQ.driverRating = driverRating,
      BeamDQ.status = status,
      BeamDQ.vehicleVariant = vehicleVariant,
      BeamDQ.distance = distance,
      BeamDQ.distanceToPickup = distanceToPickup,
      BeamDQ.durationToPickup = durationToPickup,
      BeamDQ.createdAt = T.utcToLocalTime (T.TimeZone (5 * 60 + 30) False "IST") createdAt,
      BeamDQ.updatedAt = T.utcToLocalTime (T.TimeZone (5 * 60 + 30) False "IST") updatedAt,
      BeamDQ.validTill = T.utcToLocalTime (T.TimeZone (5 * 60 + 30) False "IST") validTill,
      BeamDQ.estimatedFare = estimatedFare,
      BeamDQ.fareParametersId = getId fareParams.id,
      BeamDQ.providerId = getId providerId,
      BeamDQ.specialLocationTag = specialLocationTag
    }
