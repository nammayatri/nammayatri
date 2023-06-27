module Storage.Queries.DriverOffer where

import Database.Beam.MySQL ()
import Domain.Types.DriverOffer
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Id
import Lib.Utils
import qualified Sequelize as Se
import qualified Storage.Beam.DriverOffer as BeamDO

createDriverOffer :: L.MonadFlow m => DriverOffer -> m (MeshResult ())
createDriverOffer driverOffer = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDO.DriverOfferT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> KV.createWoReturingKVConnector dbConf' updatedMeshConfig (transformDomainDriverOfferToBeam driverOffer)
    Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

findById :: L.MonadFlow m => Id DriverOffer -> m (Maybe DriverOffer)
findById (Id driverOfferId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDO.DriverOfferT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamDriverOfferToDomain <$>) <$> KV.findWithKVConnector dbCOnf' updatedMeshConfig [Se.Is BeamDO.id $ Se.Eq driverOfferId]
    Nothing -> pure Nothing

findByBPPQuoteId :: L.MonadFlow m => Id BPPQuote -> m [DriverOffer]
findByBPPQuoteId (Id bppQuoteId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDO.DriverOfferT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> either (pure []) (transformBeamDriverOfferToDomain <$>) <$> KV.findAllWithKVConnector dbCOnf' updatedMeshConfig [Se.Is BeamDO.bppQuoteId $ Se.Eq bppQuoteId]
    Nothing -> pure []

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
      rating = rating
    }

transformDomainDriverOfferToBeam :: DriverOffer -> BeamDO.DriverOffer
transformDomainDriverOfferToBeam DriverOffer {..} =
  BeamDO.defaultDriverOffer
    { BeamDO.id = getId id,
      BeamDO.estimateId = getId estimateId,
      BeamDO.merchantId = getId <$> merchantId,
      BeamDO.driverName = driverName,
      BeamDO.durationToPickup = durationToPickup,
      BeamDO.distanceToPickup = distanceToPickup,
      BeamDO.validTill = validTill,
      BeamDO.bppQuoteId = getId bppQuoteId,
      BeamDO.rating = rating
    }
