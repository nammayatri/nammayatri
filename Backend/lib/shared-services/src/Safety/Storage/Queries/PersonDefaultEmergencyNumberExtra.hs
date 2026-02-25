{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Safety.Storage.Queries.PersonDefaultEmergencyNumberExtra where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Safety.Domain.Types.Common as SafetyCommon
import qualified Safety.Domain.Types.PersonDefaultEmergencyNumber as PDEN
import qualified Safety.Storage.Beam.PersonDefaultEmergencyNumber as BeamPDEN
import Safety.Storage.BeamFlow
import Safety.Storage.Queries.OrphanInstances.PersonDefaultEmergencyNumber ()
import qualified Sequelize as Se

create :: (BeamFlow m r) => PDEN.PersonDefaultEmergencyNumber -> m ()
create = createWithKV

createMany :: (BeamFlow m r) => [PDEN.PersonDefaultEmergencyNumber] -> m ()
createMany = traverse_ create

replaceAll :: (BeamFlow m r) => Id SafetyCommon.Person -> [PDEN.PersonDefaultEmergencyNumber] -> m ()
replaceAll (Id personId) pdenList = do
  deleteWithKV [Se.Is BeamPDEN.personId $ Se.Eq personId]
  createMany pdenList

findAllByPersonId :: (BeamFlow m r) => Id SafetyCommon.Person -> m [PDEN.PersonDefaultEmergencyNumber]
findAllByPersonId (Id personId) = findAllWithKV [Se.Is BeamPDEN.personId $ Se.Eq personId]

findAllByContactPersonId :: (BeamFlow m r) => Id SafetyCommon.Person -> m [PDEN.PersonDefaultEmergencyNumber]
findAllByContactPersonId (Id contactPersonId) = findAllWithKV [Se.Is BeamPDEN.contactPersonId $ Se.Eq (Just contactPersonId)]

updateEmergencyContactPersonId :: (BeamFlow m r) => DbHash -> Id SafetyCommon.Person -> Id SafetyCommon.Merchant -> m ()
updateEmergencyContactPersonId dbHash (Id personId) (Id merchantId) = do
  updateWithKV
    [ Se.Set BeamPDEN.contactPersonId (Just personId)
    ]
    [ Se.Is BeamPDEN.mobileNumberHash $ Se.Eq dbHash,
      Se.Is BeamPDEN.merchantId $ Se.Eq (Just merchantId)
    ]

updateShareRide :: (BeamFlow m r) => DbHash -> Id SafetyCommon.Person -> Bool -> m ()
updateShareRide dbHash (Id personId) enableForShareRide = do
  updateWithKV
    [ Se.Set BeamPDEN.enableForShareRide enableForShareRide
    ]
    [ Se.Is BeamPDEN.mobileNumberHash $ Se.Eq dbHash,
      Se.Is BeamPDEN.personId $ Se.Eq personId
    ]

updateShareRideForAll :: (BeamFlow m r) => Id SafetyCommon.Person -> Bool -> m ()
updateShareRideForAll (Id personId) enableForShareRide = do
  updateWithKV
    [ Se.Set BeamPDEN.enableForShareRide enableForShareRide
    ]
    [ Se.Is BeamPDEN.personId $ Se.Eq personId
    ]

updateShareTripWithEmergencyContactOptions :: (BeamFlow m r) => Id SafetyCommon.Person -> Maybe SafetyCommon.RideShareOptions -> m ()
updateShareTripWithEmergencyContactOptions (Id personId) shareTripWithEmergencyContactOption = do
  updateWithKV
    [ Se.Set BeamPDEN.shareTripWithEmergencyContactOption shareTripWithEmergencyContactOption
    ]
    [ Se.Is BeamPDEN.personId $ Se.Eq personId
    ]
