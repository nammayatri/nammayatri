{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.FleetDriverAssociation where

import Domain.Types.FleetDriverAssociation
import Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id as KTI
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.FleetDriverAssociation as BeamFDVA

create :: KvDbFlow m r => FleetDriverAssociation -> m ()
create = createWithKV

findByDriverIdAndFleetOwnerId :: KvDbFlow m r => Id Person -> Text -> m (Maybe FleetDriverAssociation)
findByDriverIdAndFleetOwnerId driverId fleetOwnerId =
  findOneWithKV
    [ Se.And
        [ Se.Is BeamFDVA.driverId $ Se.Eq driverId.getId,
          Se.Is BeamFDVA.fleetOwnerId $ Se.Eq fleetOwnerId
        ]
    ]

upsert :: KvDbFlow m r => FleetDriverAssociation -> m ()
upsert a@FleetDriverAssociation {..} = do
  res <- findOneWithKV [Se.And [Se.Is BeamFDVA.driverId $ Se.Eq (a.driverId.getId), Se.Is BeamFDVA.fleetOwnerId $ Se.Eq a.fleetOwnerId]]
  if isJust res
    then
      updateOneWithKV
        [ Se.Set BeamFDVA.isActive isActive,
          Se.Set BeamFDVA.updatedAt updatedAt
        ]
        [Se.And [Se.Is BeamFDVA.driverId $ Se.Eq (a.driverId.getId), Se.Is BeamFDVA.fleetOwnerId $ Se.Eq a.fleetOwnerId]]
    else createWithKV a

findAllActiveDriverByFleetOwnerId :: KvDbFlow m r => Text -> Int -> Int -> m [FleetDriverAssociation]
findAllActiveDriverByFleetOwnerId fleetOwnerId limit offset = do
  findAllWithOptionsKV
    [Se.And [Se.Is BeamFDVA.fleetOwnerId $ Se.Eq fleetOwnerId, Se.Is BeamFDVA.isActive $ Se.Eq True]]
    (Se.Desc BeamFDVA.updatedAt)
    (Just limit)
    (Just offset)

findAllDriverByFleetOwnerIdAndMbIsActive ::
  KvDbFlow m r =>
  Text ->
  Maybe Bool ->
  Int ->
  Int ->
  m [FleetDriverAssociation]
findAllDriverByFleetOwnerIdAndMbIsActive fleetOwnerId mbIsActive limit offset = do
  findAllWithOptionsKV
    [ Se.And $
        [Se.Is BeamFDVA.fleetOwnerId $ Se.Eq fleetOwnerId]
          <> [Se.Is BeamFDVA.isActive (Se.Eq isActive) | Just isActive <- [mbIsActive]]
    ]
    (Se.Desc BeamFDVA.updatedAt)
    (Just limit)
    (Just offset)

findAllDriverByFleetOwnerId' :: KvDbFlow m r => Text -> m [FleetDriverAssociation]
findAllDriverByFleetOwnerId' fleetOwnerId = do
  findAllWithKV
    [Se.Is BeamFDVA.fleetOwnerId $ Se.Eq fleetOwnerId]

updateFleetDriverActiveStatus :: KvDbFlow m r => Text -> Id Person -> Bool -> m ()
updateFleetDriverActiveStatus fleetOwnerId driverId isActive = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamFDVA.isActive isActive,
      Se.Set BeamFDVA.associatedTill $ Just now,
      Se.Set BeamFDVA.updatedAt now
    ]
    [Se.And [Se.Is BeamFDVA.driverId (Se.Eq driverId.getId), Se.Is BeamFDVA.fleetOwnerId (Se.Eq fleetOwnerId)]]

deleteByDriverId :: KvDbFlow m r => Id Person -> m ()
deleteByDriverId driverId = do
  deleteWithKV [Se.Is BeamFDVA.driverId $ Se.Eq driverId.getId]

endFleetDriverAssociation :: KvDbFlow m r => Text -> Id Person -> m ()
endFleetDriverAssociation fleetOwnerId (Id driverId) = do
  now <- getCurrentTime
  updateWithKV
    [Se.Set BeamFDVA.associatedTill $ Just now, Se.Set BeamFDVA.isActive False]
    [Se.And [Se.Is BeamFDVA.fleetOwnerId (Se.Eq fleetOwnerId), Se.Is BeamFDVA.associatedTill (Se.GreaterThan $ Just now), Se.Is BeamFDVA.driverId (Se.Eq driverId)]]

instance FromTType' BeamFDVA.FleetDriverAssociation FleetDriverAssociation where
  fromTType' BeamFDVA.FleetDriverAssociationT {..} = do
    pure $
      Just
        FleetDriverAssociation
          { id = Id id,
            driverId = Id driverId,
            ..
          }

instance ToTType' BeamFDVA.FleetDriverAssociation FleetDriverAssociation where
  toTType' FleetDriverAssociation {..} = do
    BeamFDVA.FleetDriverAssociationT
      { BeamFDVA.id = getId id,
        BeamFDVA.driverId = getId driverId,
        BeamFDVA.fleetOwnerId = fleetOwnerId,
        BeamFDVA.isActive = isActive,
        BeamFDVA.associatedOn = associatedOn,
        BeamFDVA.associatedTill = associatedTill,
        BeamFDVA.createdAt = createdAt,
        BeamFDVA.updatedAt = updatedAt
      }
