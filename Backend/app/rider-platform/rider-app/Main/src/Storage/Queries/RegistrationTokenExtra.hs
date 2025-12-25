module Storage.Queries.RegistrationTokenExtra where

import qualified Domain.Types.PartnerOrganization as DPOrg
import Domain.Types.Person
import Domain.Types.RegistrationToken
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.RegistrationToken as BeamRT
import Storage.Queries.OrphanInstances.RegistrationToken ()

-- Extra code goes here --

deleteByPersonIdExceptNew :: (MonadFlow m, EsqDBFlow m r) => Id Person -> Id RegistrationToken -> m ()
deleteByPersonIdExceptNew (Id personId) (Id newRT) = deleteWithKV [Se.And [Se.Is BeamRT.entityId (Se.Eq personId), Se.Is BeamRT.id (Se.Not $ Se.Eq newRT)]]

findByToken :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => RegToken -> m (Maybe RegistrationToken)
findByToken token = findOneWithKV [Se.Is BeamRT.token $ Se.Eq token]

deleteByPersonId :: (MonadFlow m, EsqDBFlow m r) => Id Person -> m ()
deleteByPersonId (Id personId) = deleteWithKV [Se.And [Se.Is BeamRT.entityId (Se.Eq personId)]]

findAllByPersonId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> m [RegistrationToken]
findAllByPersonId personId = findAllWithKV [Se.Is BeamRT.entityId $ Se.Eq $ getId personId]

setDirectAuth :: (MonadFlow m, EsqDBFlow m r) => Id RegistrationToken -> Medium -> m ()
setDirectAuth (Id rtId) authMedium = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamRT.verified True,
      Se.Set BeamRT.authMedium authMedium,
      Se.Set BeamRT.authType DIRECT,
      Se.Set BeamRT.updatedAt now
    ]
    [Se.Is BeamRT.id (Se.Eq rtId)]

deleteByTokenCreatedViaPartnerOrgId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => RegToken -> Id DPOrg.PartnerOrganization -> m ()
deleteByTokenCreatedViaPartnerOrgId token (Id orgId) =
  deleteWithKV
    [ Se.And
        [ Se.Is BeamRT.token $ Se.Eq token,
          Se.Is BeamRT.createdViaPartnerOrgId $ Se.Eq (Just orgId)
        ]
    ]

updateOtpByIdForPartnerOrgId :: (MonadFlow m, EsqDBFlow m r) => Id RegistrationToken -> Id DPOrg.PartnerOrganization -> Text -> Int -> m ()
updateOtpByIdForPartnerOrgId (Id rtId) (Id orgId) authValueHash authExpiry = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamRT.authValueHash authValueHash,
      Se.Set BeamRT.updatedAt now,
      Se.Set BeamRT.verified False,
      Se.Set BeamRT.authExpiry authExpiry
    ]
    [ Se.And
        [ Se.Is BeamRT.id (Se.Eq rtId),
          Se.Is BeamRT.createdViaPartnerOrgId $ Se.Eq (Just orgId)
        ]
    ]

updateOtpAndToken :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id RegistrationToken -> Text -> RegToken -> Int -> UTCTime -> m ()
updateOtpAndToken (Id rtId) newOtp newToken newAttempts now = do
  updateOneWithKV
    [ Se.Set BeamRT.authValueHash newOtp,
      Se.Set BeamRT.token newToken,
      Se.Set BeamRT.attempts newAttempts,
      Se.Set BeamRT.updatedAt now
    ]
    [Se.Is BeamRT.id (Se.Eq rtId)]
