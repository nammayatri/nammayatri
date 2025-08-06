{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.JourneyLegExtra where

import Domain.Types.FRFSRouteDetails
import qualified Domain.Types.Journey as Journey
import Domain.Types.JourneyLeg
import qualified Domain.Types.JourneyLeg as JL
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
import Storage.Queries.OrphanInstances.JourneyLeg
import qualified Storage.Queries.RouteDetails as RD

-- Extra code goes here --
create' :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.JourneyLeg.JourneyLeg -> m ())
create' = createWithKV

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.JourneyLeg.JourneyLeg -> m ())
create journeyLeg = do
  forM_ (JL.routeDetails journeyLeg) $ \routeDetail -> do
    RD.create routeDetail
  create' journeyLeg

getJourneyLegs :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Kernel.Types.Id.Id Journey.Journey -> m [Domain.Types.JourneyLeg.JourneyLeg]
getJourneyLegs journeyId = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.journeyId $ Se.Eq (Kernel.Types.Id.getId journeyId),
          Se.Or
            [Se.Is Beam.isDeleted $ Se.Eq (Just False), Se.Is Beam.isDeleted $ Se.Eq Nothing]
        ]
    ]
    (Se.Asc Beam.sequenceNumber)
    Nothing
    Nothing
