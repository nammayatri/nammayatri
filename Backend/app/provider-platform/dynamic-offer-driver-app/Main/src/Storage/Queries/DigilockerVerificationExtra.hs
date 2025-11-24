{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DigilockerVerificationExtra where

import qualified Domain.Types.DigilockerVerification as DDV
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DigilockerVerification as Beam
import Storage.Queries.OrphanInstances.DigilockerVerification ()

-- Override updateAccessToken to use EncryptedField instead of EncryptedHashedField
-- since we don't need hash for searching
updateAccessToken ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Maybe (EncryptedField 'AsEncrypted Text) ->
  Maybe UTCTime ->
  Maybe Text ->
  Maybe Text ->
  Text ->
  m ()
updateAccessToken accessToken accessTokenExpiresAt authorizationCode scope stateId = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.accessToken (accessToken <&> unEncrypted),
      Se.Set Beam.accessTokenExpiresAt accessTokenExpiresAt,
      Se.Set Beam.authorizationCode authorizationCode,
      Se.Set Beam.scope scope,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.stateId $ Se.Eq stateId]
