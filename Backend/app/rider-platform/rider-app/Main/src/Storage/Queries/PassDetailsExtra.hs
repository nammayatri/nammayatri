{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PassDetailsExtra where

import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.PassDetails as DPassDetails
import qualified Domain.Types.PassOrganization as DPassOrganization
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.PassDetails as Beam
import Storage.Queries.OrphanInstances.PassDetails ()

getLastReferenceNumber ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  m Int
getLastReferenceNumber = do
  (passDetails :: [DPassDetails.PassDetails]) <-
    findAllWithOptionsDb
      [Se.Is Beam.referenceNumber $ Se.Not $ Se.Eq Nothing]
      (Se.Desc Beam.referenceNumber)
      (Just 1)
      (Just 0)
  pure $ case listToMaybe passDetails of
    Just pd -> fromMaybe 0 pd.referenceNumber
    Nothing -> 0

-- Hand-written: the DSL's `in:` on a Maybe column generates a wrong getId mapping.
findAllByPassOrganizationIdsAndVerificationStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Maybe Int ->
  Maybe Int ->
  [Id DPassOrganization.PassOrganization] ->
  [DPassDetails.VerificationStatus] ->
  Id DMOC.MerchantOperatingCity ->
  m [DPassDetails.PassDetails]
findAllByPassOrganizationIdsAndVerificationStatus limit offset passOrganizationIds verificationStatus merchantOperatingCityId =
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.passOrganizationId $ Se.In (Just . getId <$> passOrganizationIds),
          Se.Is Beam.verificationStatus $ Se.In verificationStatus,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (getId merchantOperatingCityId)
        ]
    ]
    (Se.Asc Beam.updatedAt)
    limit
    offset
