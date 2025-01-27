module Storage.Queries.WalkLegMultimodalExtra where

import Domain.Types.Journey
import qualified Domain.Types.LocationMapping as DLM
import Domain.Types.WalkLegMultimodal
import qualified EulerHS.Prelude as EP
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified SharedLogic.LocationMapping as SLM
import qualified Storage.Beam.WalkLegMultimodal as Beam
import qualified Storage.Queries.Location as QL
import qualified Storage.Queries.LocationMapping as QLM
import Storage.Queries.OrphanInstances.WalkLegMultimodal ()

-- Extra code goes here --

createWalkLeg' :: (MonadFlow m, EsqDBFlow m r) => WalkLegMultimodal -> m ()
createWalkLeg' = createWithKV

create' :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => WalkLegMultimodal -> m ()
create' walkLeg = do
  _ <- EP.whenNothingM_ (QL.findById walkLeg.fromLocation.id) $ do QL.create walkLeg.fromLocation
  _ <- whenJust walkLeg.toLocation $ \location -> processLocation location
  createWalkLeg' walkLeg
  where
    processLocation location = EP.whenNothingM_ (QL.findById location.id) $ do QL.create location

createWalkLeg :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => WalkLegMultimodal -> m ()
createWalkLeg walkLeg = do
  fromLocationMap <- SLM.buildPickUpLocationMapping walkLeg.fromLocation.id walkLeg.id.getId DLM.WALK_LEG (Just walkLeg.merchantId) (Just walkLeg.merchantOperatingCityId)
  void $ QLM.create fromLocationMap
  mbToLocationMap <- maybe (pure Nothing) (\detail -> Just <$> SLM.buildDropLocationMapping detail.id walkLeg.id.getId DLM.WALK_LEG (Just walkLeg.merchantId) (Just walkLeg.merchantOperatingCityId)) walkLeg.toLocation
  void $ whenJust mbToLocationMap $ \toLocMap -> QLM.create toLocMap
  create' walkLeg

findAllByJourneyId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Kernel.Types.Id.Id Domain.Types.Journey.Journey -> m [Domain.Types.WalkLegMultimodal.WalkLegMultimodal]
findAllByJourneyId journeyId =
  findAllWithKVAndConditionalDB
    [Se.Is Beam.journeyId $ Se.Eq (Just journeyId.getId)]
    Nothing

updateIsCancelled :: (MonadFlow m, EsqDBFlow m r) => Id WalkLegMultimodal -> Maybe Bool -> m ()
updateIsCancelled (Id reqId) isDeleted = do
  updateOneWithKV
    [Se.Set Beam.isDeleted isDeleted]
    [Se.Is Beam.id (Se.Eq reqId)]
