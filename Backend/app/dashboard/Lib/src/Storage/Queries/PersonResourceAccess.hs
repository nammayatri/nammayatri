{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.PersonResourceAccess where

import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.Person as DP
import qualified Domain.Types.ResourceScope as DRS
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Id
import Sequelize as Se
import Storage.Beam.BeamFlow
import qualified Storage.Beam.PersonResourceAccess as BeamPRA

create :: BeamFlow m r => DRS.PersonResourceAccess -> m ()
create = createWithKV

-- | Every resource-scope row for a person, across all merchants/cities/types.
-- Used by /user/resourceScope and the admin "view assignments" endpoint.
findAllByPersonId :: BeamFlow m r => Id DP.Person -> m [DRS.PersonResourceAccess]
findAllByPersonId personId =
  findAllWithKV [Se.Is BeamPRA.personId $ Se.Eq $ getId personId]

findByPersonMerchantCityType ::
  BeamFlow m r =>
  Id DP.Person ->
  Id DMerchant.Merchant ->
  City.City ->
  DRS.ResourceType ->
  m [DRS.PersonResourceAccess]
findByPersonMerchantCityType personId merchantId city resourceType =
  findAllWithKV
    [ Se.And
        [ Se.Is BeamPRA.personId $ Se.Eq $ getId personId,
          Se.Is BeamPRA.merchantId $ Se.Eq $ getId merchantId,
          Se.Is BeamPRA.operatingCity $ Se.Eq city,
          Se.Is BeamPRA.resourceType $ Se.Eq resourceType
        ]
    ]

-- | The resource ids a person may act on for a (merchant, city, type). A
-- DRS.wildcardResourceId among them means full-MOC; empty means deny-all.
findResourceIds ::
  BeamFlow m r =>
  Id DP.Person ->
  Id DMerchant.Merchant ->
  City.City ->
  DRS.ResourceType ->
  m [Text]
findResourceIds personId merchantId city resourceType =
  map (.resourceId) <$> findByPersonMerchantCityType personId merchantId city resourceType

-- | Clear a person's assignment for one (merchant, city, type) before re-writing
-- it — assign does reset-then-insert so the row set is exactly what was sent.
deleteByPersonMerchantCityType ::
  BeamFlow m r =>
  Id DP.Person ->
  Id DMerchant.Merchant ->
  City.City ->
  DRS.ResourceType ->
  m ()
deleteByPersonMerchantCityType personId merchantId city resourceType =
  deleteWithKV
    [ Se.And
        [ Se.Is BeamPRA.personId $ Se.Eq $ getId personId,
          Se.Is BeamPRA.merchantId $ Se.Eq $ getId merchantId,
          Se.Is BeamPRA.operatingCity $ Se.Eq city,
          Se.Is BeamPRA.resourceType $ Se.Eq resourceType
        ]
    ]

-- | Remove all of a departing person's rows before the person row is deleted
-- (person_resource_access.person_id is a NOT NULL FK to person).
deleteAllByPersonId :: BeamFlow m r => Id DP.Person -> m ()
deleteAllByPersonId personId =
  deleteWithKV [Se.Is BeamPRA.personId $ Se.Eq $ getId personId]

instance FromTType' BeamPRA.PersonResourceAccess DRS.PersonResourceAccess where
  fromTType' BeamPRA.PersonResourceAccessT {..} =
    return $
      Just
        DRS.PersonResourceAccess
          { id = Id id,
            personId = Id personId,
            merchantId = Id merchantId,
            ..
          }

instance ToTType' BeamPRA.PersonResourceAccess DRS.PersonResourceAccess where
  toTType' DRS.PersonResourceAccess {..} =
    BeamPRA.PersonResourceAccessT
      { id = getId id,
        personId = getId personId,
        merchantId = getId merchantId,
        ..
      }
