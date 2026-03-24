{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.UserPreferredRoute where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.UserPreferredRoute
import qualified Storage.Beam.UserPreferredRoute as Beam
import qualified Kernel.Prelude
import qualified Domain.Types.LocationAddress
import qualified Kernel.Types.Id
import qualified Domain.Types.Person
import qualified Sequelize as Se



create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.UserPreferredRoute.UserPreferredRoute -> m ())
create = createWithKV
createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.UserPreferredRoute.UserPreferredRoute] -> m ())
createMany = traverse_ create
findAllByPersonId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                     (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ([Domain.Types.UserPreferredRoute.UserPreferredRoute]))
findAllByPersonId limit offset personId = do findAllWithOptionsKV [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)] (Se.Asc Beam.priority) limit offset
findByIdAndPersonId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                       (Kernel.Types.Id.Id Domain.Types.UserPreferredRoute.UserPreferredRoute -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.UserPreferredRoute.UserPreferredRoute))
findByIdAndPersonId id personId = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id), Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]]
updateUsageCount :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                    (Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.UserPreferredRoute.UserPreferredRoute -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateUsageCount usageCount id personId = do {_now <- getCurrentTime;
                                              updateOneWithKV [Se.Set Beam.usageCount usageCount, Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id),
                                                                                                                                       Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]]}
findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                    (Kernel.Types.Id.Id Domain.Types.UserPreferredRoute.UserPreferredRoute -> m (Maybe Domain.Types.UserPreferredRoute.UserPreferredRoute))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.UserPreferredRoute.UserPreferredRoute -> m ())
updateByPrimaryKey (Domain.Types.UserPreferredRoute.UserPreferredRoute {..}) = do {_now <- getCurrentTime;
                                                                                   updateWithKV [Se.Set Beam.fromArea (Domain.Types.LocationAddress.area fromLocationAddress),
                                                                                                 Se.Set Beam.fromAreaCode (Domain.Types.LocationAddress.areaCode fromLocationAddress),
                                                                                                 Se.Set Beam.fromBuilding (Domain.Types.LocationAddress.building fromLocationAddress),
                                                                                                 Se.Set Beam.fromCity (Domain.Types.LocationAddress.city fromLocationAddress),
                                                                                                 Se.Set Beam.fromCountry (Domain.Types.LocationAddress.country fromLocationAddress),
                                                                                                 Se.Set Beam.fromDoor (Domain.Types.LocationAddress.door fromLocationAddress),
                                                                                                 Se.Set Beam.fromExtras (Domain.Types.LocationAddress.extras fromLocationAddress),
                                                                                                 Se.Set Beam.fromInstructions (Domain.Types.LocationAddress.instructions fromLocationAddress),
                                                                                                 Se.Set Beam.fromPlaceId (Domain.Types.LocationAddress.placeId fromLocationAddress),
                                                                                                 Se.Set Beam.fromState (Domain.Types.LocationAddress.state fromLocationAddress),
                                                                                                 Se.Set Beam.fromStreet (Domain.Types.LocationAddress.street fromLocationAddress),
                                                                                                 Se.Set Beam.fromTitle (Domain.Types.LocationAddress.title fromLocationAddress),
                                                                                                 Se.Set Beam.fromWard (Domain.Types.LocationAddress.ward fromLocationAddress),
                                                                                                 Se.Set Beam.fromLocationLat fromLocationLat,
                                                                                                 Se.Set Beam.fromLocationLon fromLocationLon,
                                                                                                 Se.Set Beam.personId (Kernel.Types.Id.getId personId),
                                                                                                 Se.Set Beam.priority priority,
                                                                                                 Se.Set Beam.routeName routeName,
                                                                                                 Se.Set Beam.toArea (Domain.Types.LocationAddress.area toLocationAddress),
                                                                                                 Se.Set Beam.toAreaCode (Domain.Types.LocationAddress.areaCode toLocationAddress),
                                                                                                 Se.Set Beam.toBuilding (Domain.Types.LocationAddress.building toLocationAddress),
                                                                                                 Se.Set Beam.toCity (Domain.Types.LocationAddress.city toLocationAddress),
                                                                                                 Se.Set Beam.toCountry (Domain.Types.LocationAddress.country toLocationAddress),
                                                                                                 Se.Set Beam.toDoor (Domain.Types.LocationAddress.door toLocationAddress),
                                                                                                 Se.Set Beam.toExtras (Domain.Types.LocationAddress.extras toLocationAddress),
                                                                                                 Se.Set Beam.toInstructions (Domain.Types.LocationAddress.instructions toLocationAddress),
                                                                                                 Se.Set Beam.toPlaceId (Domain.Types.LocationAddress.placeId toLocationAddress),
                                                                                                 Se.Set Beam.toState (Domain.Types.LocationAddress.state toLocationAddress),
                                                                                                 Se.Set Beam.toStreet (Domain.Types.LocationAddress.street toLocationAddress),
                                                                                                 Se.Set Beam.toTitle (Domain.Types.LocationAddress.title toLocationAddress),
                                                                                                 Se.Set Beam.toWard (Domain.Types.LocationAddress.ward toLocationAddress),
                                                                                                 Se.Set Beam.toLocationLat toLocationLat,
                                                                                                 Se.Set Beam.toLocationLon toLocationLon,
                                                                                                 Se.Set Beam.updatedAt _now,
                                                                                                 Se.Set Beam.usageCount usageCount] [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]}



instance FromTType' Beam.UserPreferredRoute Domain.Types.UserPreferredRoute.UserPreferredRoute
    where fromTType' (Beam.UserPreferredRouteT {..}) = do pure $ Just Domain.Types.UserPreferredRoute.UserPreferredRoute{createdAt = createdAt,
                                                                                                                         fromLocationAddress = Domain.Types.LocationAddress.LocationAddress {street = fromStreet, door = fromDoor, city = fromCity, state = fromState, country = fromCountry, building = fromBuilding, areaCode = fromAreaCode, area = fromArea, ward = fromWard, placeId = fromPlaceId, instructions = fromInstructions, title = fromTitle, extras = fromExtras},
                                                                                                                         fromLocationLat = fromLocationLat,
                                                                                                                         fromLocationLon = fromLocationLon,
                                                                                                                         id = Kernel.Types.Id.Id id,
                                                                                                                         personId = Kernel.Types.Id.Id personId,
                                                                                                                         priority = priority,
                                                                                                                         routeName = routeName,
                                                                                                                         toLocationAddress = Domain.Types.LocationAddress.LocationAddress {street = toStreet, door = toDoor, city = toCity, state = toState, country = toCountry, building = toBuilding, areaCode = toAreaCode, area = toArea, ward = toWard, placeId = toPlaceId, instructions = toInstructions, title = toTitle, extras = toExtras},
                                                                                                                         toLocationLat = toLocationLat,
                                                                                                                         toLocationLon = toLocationLon,
                                                                                                                         updatedAt = updatedAt,
                                                                                                                         usageCount = usageCount}
instance ToTType' Beam.UserPreferredRoute Domain.Types.UserPreferredRoute.UserPreferredRoute
    where toTType' (Domain.Types.UserPreferredRoute.UserPreferredRoute {..}) = do Beam.UserPreferredRouteT{Beam.createdAt = createdAt,
                                                                                                           Beam.fromArea = Domain.Types.LocationAddress.area fromLocationAddress,
                                                                                                           Beam.fromAreaCode = Domain.Types.LocationAddress.areaCode fromLocationAddress,
                                                                                                           Beam.fromBuilding = Domain.Types.LocationAddress.building fromLocationAddress,
                                                                                                           Beam.fromCity = Domain.Types.LocationAddress.city fromLocationAddress,
                                                                                                           Beam.fromCountry = Domain.Types.LocationAddress.country fromLocationAddress,
                                                                                                           Beam.fromDoor = Domain.Types.LocationAddress.door fromLocationAddress,
                                                                                                           Beam.fromExtras = Domain.Types.LocationAddress.extras fromLocationAddress,
                                                                                                           Beam.fromInstructions = Domain.Types.LocationAddress.instructions fromLocationAddress,
                                                                                                           Beam.fromPlaceId = Domain.Types.LocationAddress.placeId fromLocationAddress,
                                                                                                           Beam.fromState = Domain.Types.LocationAddress.state fromLocationAddress,
                                                                                                           Beam.fromStreet = Domain.Types.LocationAddress.street fromLocationAddress,
                                                                                                           Beam.fromTitle = Domain.Types.LocationAddress.title fromLocationAddress,
                                                                                                           Beam.fromWard = Domain.Types.LocationAddress.ward fromLocationAddress,
                                                                                                           Beam.fromLocationLat = fromLocationLat,
                                                                                                           Beam.fromLocationLon = fromLocationLon,
                                                                                                           Beam.id = Kernel.Types.Id.getId id,
                                                                                                           Beam.personId = Kernel.Types.Id.getId personId,
                                                                                                           Beam.priority = priority,
                                                                                                           Beam.routeName = routeName,
                                                                                                           Beam.toArea = Domain.Types.LocationAddress.area toLocationAddress,
                                                                                                           Beam.toAreaCode = Domain.Types.LocationAddress.areaCode toLocationAddress,
                                                                                                           Beam.toBuilding = Domain.Types.LocationAddress.building toLocationAddress,
                                                                                                           Beam.toCity = Domain.Types.LocationAddress.city toLocationAddress,
                                                                                                           Beam.toCountry = Domain.Types.LocationAddress.country toLocationAddress,
                                                                                                           Beam.toDoor = Domain.Types.LocationAddress.door toLocationAddress,
                                                                                                           Beam.toExtras = Domain.Types.LocationAddress.extras toLocationAddress,
                                                                                                           Beam.toInstructions = Domain.Types.LocationAddress.instructions toLocationAddress,
                                                                                                           Beam.toPlaceId = Domain.Types.LocationAddress.placeId toLocationAddress,
                                                                                                           Beam.toState = Domain.Types.LocationAddress.state toLocationAddress,
                                                                                                           Beam.toStreet = Domain.Types.LocationAddress.street toLocationAddress,
                                                                                                           Beam.toTitle = Domain.Types.LocationAddress.title toLocationAddress,
                                                                                                           Beam.toWard = Domain.Types.LocationAddress.ward toLocationAddress,
                                                                                                           Beam.toLocationLat = toLocationLat,
                                                                                                           Beam.toLocationLon = toLocationLon,
                                                                                                           Beam.updatedAt = updatedAt,
                                                                                                           Beam.usageCount = usageCount}



