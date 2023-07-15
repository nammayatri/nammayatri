{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.DriverOffer where

import Database.Beam.MySQL ()
import Domain.Types.DriverOffer
import Domain.Types.Estimate
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common
import Kernel.Types.Id
import Lib.Utils
import qualified Sequelize as Se
import qualified Storage.Beam.DriverOffer as BeamDO

createDriverOffer :: L.MonadFlow m => DriverOffer -> m (MeshResult ())
createDriverOffer driverOffer = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDO.DriverOfferT
  updatedMeshConfig <- setMeshConfig modelName
  case dbConf of
    Just dbConf' -> KV.createWoReturingKVConnector dbConf' updatedMeshConfig (transformDomainDriverOfferToBeam driverOffer)
    Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

findById :: L.MonadFlow m => Id DriverOffer -> m (Maybe DriverOffer)
findById (Id driverOfferId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDO.DriverOfferT
  updatedMeshConfig <- setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamDriverOfferToDomain <$>) <$> KV.findWithKVConnector dbCOnf' updatedMeshConfig [Se.Is BeamDO.id $ Se.Eq driverOfferId]
    Nothing -> pure Nothing

findByBPPQuoteId :: L.MonadFlow m => Id BPPQuote -> m [DriverOffer]
findByBPPQuoteId (Id bppQuoteId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDO.DriverOfferT
  updatedMeshConfig <- setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> either (pure []) (transformBeamDriverOfferToDomain <$>) <$> KV.findAllWithKVConnector dbCOnf' updatedMeshConfig [Se.Is BeamDO.bppQuoteId $ Se.Eq bppQuoteId]
    Nothing -> pure []

-- updateStatus :: Id Estimate -> DriverOfferStatus -> SqlDB ()
-- updateStatus estimateId status = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ DriverOfferStatus =. val status,
--         DriverOfferUpdatedAt =. val now
--       ]
--     where_ $ tbl ^. DriverOfferEstimateId ==. val (toKey estimateId)

updateStatus :: (L.MonadFlow m, MonadTime m) => Id Estimate -> DriverOfferStatus -> m (MeshResult ())
updateStatus (Id estimateId) status_ = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDO.DriverOfferT
  updatedMeshConfig <- setMeshConfig modelName
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        updatedMeshConfig
        [ Se.Set BeamDO.updatedAt now,
          Se.Set BeamDO.status status_
        ]
        [Se.Is BeamDO.id (Se.Eq estimateId)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

transformBeamDriverOfferToDomain :: BeamDO.DriverOffer -> DriverOffer
transformBeamDriverOfferToDomain BeamDO.DriverOfferT {..} = do
  DriverOffer
    { id = Id id,
      estimateId = Id estimateId,
      merchantId = Id <$> merchantId,
      driverName = driverName,
      durationToPickup = durationToPickup,
      distanceToPickup = distanceToPickup,
      validTill = validTill,
      bppQuoteId = Id bppQuoteId,
      rating = rating,
      status = status,
      updatedAt = updatedAt
    }

transformDomainDriverOfferToBeam :: DriverOffer -> BeamDO.DriverOffer
transformDomainDriverOfferToBeam DriverOffer {..} =
  BeamDO.DriverOfferT
    { BeamDO.id = getId id,
      BeamDO.estimateId = getId estimateId,
      BeamDO.merchantId = getId <$> merchantId,
      BeamDO.driverName = driverName,
      BeamDO.durationToPickup = durationToPickup,
      BeamDO.distanceToPickup = distanceToPickup,
      BeamDO.validTill = validTill,
      BeamDO.bppQuoteId = getId bppQuoteId,
      BeamDO.rating = rating,
      BeamDO.status = status,
      BeamDO.updatedAt = updatedAt
    }
