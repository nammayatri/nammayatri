{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PersonDefaultEmergencyNumberExtra where

import qualified Domain.Types.Merchant as DMerchant
import Domain.Types.Person
import Domain.Types.PersonDefaultEmergencyNumber
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.PersonDefaultEmergencyNumber as BeamPDEN
import Storage.Queries.OrphanInstances.PersonDefaultEmergencyNumber

create :: KvDbFlow m r => PersonDefaultEmergencyNumber -> m ()
create = createWithKV

createMany :: KvDbFlow m r => [PersonDefaultEmergencyNumber] -> m ()
createMany = traverse_ create

replaceAll :: KvDbFlow m r => Id Person -> [PersonDefaultEmergencyNumber] -> m ()
replaceAll (Id personId) pdenList = do
  deleteWithKV [Se.Is BeamPDEN.personId $ Se.Eq personId]
  createMany pdenList

findAllByPersonId :: KvDbFlow m r => Id Person -> m [PersonDefaultEmergencyNumber]
findAllByPersonId (Id personId) = findAllWithKV [Se.Is BeamPDEN.personId $ Se.Eq personId]

findAllByContactPersonId :: KvDbFlow m r => Id Person -> m [PersonDefaultEmergencyNumber]
findAllByContactPersonId (Id contactPersonId) = findAllWithKV [Se.Is BeamPDEN.contactPersonId $ Se.Eq (Just contactPersonId)]

updateEmergencyContactPersonId :: KvDbFlow m r => DbHash -> Id Person -> Id DMerchant.Merchant -> m ()
updateEmergencyContactPersonId dbHash (Id personId) (Id merchantId) = do
  updateWithKV
    [ Se.Set BeamPDEN.contactPersonId (Just personId)
    ]
    [ Se.Is BeamPDEN.mobileNumberHash $ Se.Eq dbHash,
      Se.Is BeamPDEN.merchantId $ Se.Eq (Just merchantId)
    ]

updateShareRide :: KvDbFlow m r => DbHash -> Id Person -> Bool -> m ()
updateShareRide dbHash (Id personId) enableForShareRide = do
  updateWithKV
    [ Se.Set BeamPDEN.enableForShareRide enableForShareRide
    ]
    [ Se.Is BeamPDEN.mobileNumberHash $ Se.Eq dbHash,
      Se.Is BeamPDEN.personId $ Se.Eq personId
    ]

updateShareRideForAll :: KvDbFlow m r => Id Person -> Bool -> m ()
updateShareRideForAll (Id personId) enableForShareRide = do
  updateWithKV
    [ Se.Set BeamPDEN.enableForShareRide enableForShareRide
    ]
    [ Se.Is BeamPDEN.personId $ Se.Eq personId
    ]
