module Storage.Queries.FleetOwnerInformationExtra where

import Domain.Types.FleetOwnerInformation (FleetOwnerInformation)
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.FleetOwnerInformation as Beam
import Storage.Queries.FleetOwnerInformation ()

findByPersonIdAndEnabledAndVerified ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Maybe Bool ->
  Maybe Bool ->
  Kernel.Types.Id.Id Domain.Types.Person.Person ->
  m (Maybe FleetOwnerInformation)
findByPersonIdAndEnabledAndVerified mbEnabled mbVerified fleetOwnerPersonId = do
  findOneWithKV
    [ Se.And $
        [Se.Is Beam.fleetOwnerPersonId $ Se.Eq (Kernel.Types.Id.getId fleetOwnerPersonId)]
          <> [Se.Is Beam.enabled $ Se.Eq (fromJust mbEnabled) | isJust mbEnabled]
          <> [Se.Is Beam.verified $ Se.Eq (fromJust mbVerified) | isJust mbVerified]
    ]

getFleetOwnerByTicketPlaceId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Maybe Text ->
  m [Domain.Types.FleetOwnerInformation.FleetOwnerInformation]
getFleetOwnerByTicketPlaceId mbTicketPlaceId = do
  findAllWithKV
    [ Se.And $
        [Se.Is Beam.enabled $ Se.Eq True]
          <> [Se.Is Beam.verified $ Se.Eq True]
          <> [Se.Is Beam.ticketPlaceId $ Se.Eq mbTicketPlaceId]
    ]
