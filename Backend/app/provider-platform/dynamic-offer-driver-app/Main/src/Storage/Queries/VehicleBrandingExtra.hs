{-# OPTIONS_GHC -Wno-unused-imports #-}

-- Growth › Vehicle Branding — hand-written extra queries (plan v6 §7 B2/B6).
--
-- UNVERIFIED DRAFT: this references generated modules (Domain.Types.VehicleBranding,
-- Storage.Beam.VehicleBranding) that only exist AFTER `, run-generator`. It cannot be
-- compiled in this environment (no nix/cabal). Run the generator, then `cabal build
-- dynamic-offer-driver-app` and fix any signature/import drift before relying on it.
--
-- Why an Extra file: merchantOperatingCityId is on the secondary-key deny list (CLAUDE.md),
-- so city/status/date-scoped reads can't be KV-keyed queries declared in the spec; they live
-- here as findAllWithOptionsKV clauses (the same approach as CommonDriverOnboardingDocumentsExtra).

module Storage.Queries.VehicleBrandingExtra
  ( VehicleBrandingFilter (..),
    emptyFilter,
    findAllForVehicleBranding,
  )
where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.VehicleBranding as DVB
import Kernel.Beam.Functions (findAllWithOptionsKV)
import Kernel.Prelude
import qualified Kernel.Types.Documents as Documents
import qualified Kernel.Types.Id as Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.VehicleBranding as Beam
import Storage.Queries.VehicleBranding ()

data VehicleBrandingFilter = VehicleBrandingFilter
  { merchantId :: Id.Id DM.Merchant,
    merchantOperatingCityId :: Id.Id DMOC.MerchantOperatingCity,
    verificationStatus :: Maybe Documents.VerificationStatus,
    category :: Maybe DVB.BrandingCategory,
    brandingType :: Maybe DVB.BrandingType,
    registrationNo :: Maybe Text,
    from :: Maybe UTCTime,
    to :: Maybe UTCTime,
    limit :: Maybe Int,
    offset :: Maybe Int
  }

emptyFilter :: Id.Id DM.Merchant -> Id.Id DMOC.MerchantOperatingCity -> VehicleBrandingFilter
emptyFilter mId mocId =
  VehicleBrandingFilter
    { merchantId = mId,
      merchantOperatingCityId = mocId,
      verificationStatus = Nothing,
      category = Nothing,
      brandingType = Nothing,
      registrationNo = Nothing,
      from = Nothing,
      to = Nothing,
      limit = Nothing,
      offset = Nothing
    }

findAllForVehicleBranding ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  VehicleBrandingFilter ->
  m [DVB.VehicleBranding]
findAllForVehicleBranding VehicleBrandingFilter {..} =
  findAllWithOptionsKV
    [ Se.And $
        [ Se.Is Beam.merchantId $ Se.Eq (Id.getId merchantId),
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Id.getId merchantOperatingCityId)
        ]
          <> maybe [] (\s -> [Se.Is Beam.verificationStatus $ Se.Eq s]) verificationStatus
          <> maybe [] (\c -> [Se.Is Beam.category $ Se.Eq c]) category
          <> maybe [] (\t -> [Se.Is Beam.brandingType $ Se.Eq t]) brandingType
          <> maybe [] (\r -> [Se.Is Beam.registrationNo $ Se.Eq r]) registrationNo
          <> maybe [] (\f -> [Se.Is Beam.createdAt $ Se.GreaterThanOrEq f]) from
          <> maybe [] (\t -> [Se.Is Beam.createdAt $ Se.LessThanOrEq t]) to
    ]
    (Se.Desc Beam.createdAt)
    limit
    offset
