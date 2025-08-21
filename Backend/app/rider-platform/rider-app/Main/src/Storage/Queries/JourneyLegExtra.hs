{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.JourneyLegExtra where

import Control.Monad.Extra (mapMaybeM)
import Domain.Types.FRFSRouteDetails
import qualified Domain.Types.Journey as Journey
import qualified Domain.Types.JourneyLeg as JL
import qualified Domain.Types.JourneyLegMapping as DJLM
import qualified Domain.Types.RouteDetails as RouteDetails
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.External.MultiModal.Interface.Types
import Kernel.Prelude
import qualified Kernel.Types.Common as Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.JourneyLeg as Beam
import qualified Storage.Queries.JourneyLegMapping as QJourneyLegMapping
import Storage.Queries.OrphanInstances.JourneyLeg
import qualified Storage.Queries.RouteDetails as RD

-- Extra code goes here --
create' :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (JL.JourneyLeg -> m ())
create' = createWithKV

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (JL.JourneyLeg -> m ())
create journeyLeg = do
  forM_ (JL.routeDetails journeyLeg) $ \routeDetail -> do
    RD.create routeDetail
  create' journeyLeg
  journeyLegMapping <- mkJourneyLegMapping
  QJourneyLegMapping.create journeyLegMapping
  where
    mkJourneyLegMapping = do
      journeyLegMappingId <- Common.generateGUID
      return $
        DJLM.JourneyLegMapping
          { id = journeyLegMappingId,
            journeyLegId = journeyLeg.id,
            journeyId = journeyLeg.journeyId,
            sequenceNumber = journeyLeg.sequenceNumber,
            isDeleted = fromMaybe False journeyLeg.isDeleted,
            merchantId = journeyLeg.merchantId,
            merchantOperatingCityId = journeyLeg.merchantOperatingCityId,
            createdAt = journeyLeg.createdAt,
            updatedAt = journeyLeg.updatedAt
          }

findByGroupCode :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> m [JL.JourneyLeg])
findByGroupCode groupCode = do
  if isNothing groupCode
    then return []
    else findAllWithKV [Se.Is Beam.groupCode $ Se.Eq groupCode]

-- TODO :: Remove Nothing, Post Release and Adoption
getJourneyLegs :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Kernel.Types.Id.Id Journey.Journey -> m [JL.JourneyLeg]
getJourneyLegs journeyId = do
  journeyLegMappings <- QJourneyLegMapping.findAllLegsMappingByJourneyId Nothing Nothing journeyId False
  if not (null journeyLegMappings)
    then do
      mapMaybeM
        ( \journeyLegMapping -> do
            findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId journeyLegMapping.journeyLegId)]
              >>= \case
                Just leg -> return $ Just leg
                Nothing -> return Nothing
        )
        journeyLegMappings
    else do
      findAllWithOptionsKV
        [ Se.And
            [ Se.Is Beam.journeyId $ Se.Eq (Just $ Kernel.Types.Id.getId journeyId),
              Se.Or
                [Se.Is Beam.isDeleted $ Se.Eq (Just False), Se.Is Beam.isDeleted $ Se.Eq Nothing]
            ]
        ]
        (Se.Asc Beam.sequenceNumber)
        Nothing
        Nothing

getJourneyLeg ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Journey.Journey ->
  Kernel.Prelude.Int ->
  m JL.JourneyLeg
getJourneyLeg journeyId sequenceNumber = do
  findByJourneyIdAndSequenceNumber journeyId sequenceNumber >>= fromMaybeM (InvalidRequest $ "Journey Leg not found with journeyId: " <> show journeyId <> " and sequenceNumber: " <> show sequenceNumber)

findByJourneyIdAndSequenceNumber ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Journey.Journey -> Kernel.Prelude.Int -> m (Maybe JL.JourneyLeg))
findByJourneyIdAndSequenceNumber journeyId sequenceNumber = do
  mbJourneyLegMapping <- QJourneyLegMapping.findByJourneyIdAndSequenceNumber journeyId sequenceNumber False
  case mbJourneyLegMapping of
    Just journeyLegMapping -> findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId journeyLegMapping.journeyLegId)]
    Nothing ->
      findOneWithKV
        [ Se.And
            [ Se.Is Beam.journeyId $ Se.Eq (Just $ Kernel.Types.Id.getId journeyId),
              Se.Is Beam.sequenceNumber $ Se.Eq (Just sequenceNumber),
              Se.Or
                [Se.Is Beam.isDeleted $ Se.Eq (Just False), Se.Is Beam.isDeleted $ Se.Eq Nothing]
            ]
        ]
