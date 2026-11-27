{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Permit.Storage.Queries.PermitApplicationExtra where

import Data.Time (Day)
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.Permit.Domain.Types.Common as DCommon
import qualified Lib.Permit.Domain.Types.PermitApplication as Domain
import qualified Lib.Permit.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Permit.Storage.Beam.PermitApplication as Beam
import Lib.Permit.Storage.Queries.OrphanInstances.PermitApplication ()
import Lib.Permit.Types (PermitType)
import qualified Sequelize as Se

findByQrTokenHash :: BeamFlow.BeamFlow m r => DbHash -> m (Maybe Domain.PermitApplication)
findByQrTokenHash tokenHash = findOneWithKV [Se.Is Beam.qrTokenHash $ Se.Eq (Just tokenHash)]

-- | One page of the review queue for a city, earliest permit date first.
findAllForReview ::
  BeamFlow.BeamFlow m r =>
  Id DCommon.MerchantOperatingCity ->
  PermitType ->
  [Domain.PermitStatus] ->
  Maybe Day ->
  Int ->
  Int ->
  m [Domain.PermitApplication]
findAllForReview city permitType statuses mbDate limit offset =
  findAllWithOptionsKV
    [ Se.And $
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq city.getId,
          Se.Is Beam.permitType $ Se.Eq permitType,
          Se.Is Beam.status $ Se.In statuses
        ]
          <> [Se.Is Beam.permitDate $ Se.Eq date | Just date <- [mbDate]]
    ]
    (Se.Asc Beam.permitDate)
    (Just limit)
    (Just offset)

-- | Submitted or approved applications of the merchant, in any city, that share a
-- conflict value on the same permit date.
findActiveClashes ::
  BeamFlow.BeamFlow m r =>
  Id DCommon.Merchant ->
  PermitType ->
  Day ->
  Maybe Text ->
  Maybe Text ->
  m [Domain.PermitApplication]
findActiveClashes merchant permitType permitDate mbPrimary mbSecondary = do
  let conflicts =
        [Se.Is Beam.primaryConflictHash $ Se.Eq (Just primary) | Just primary <- [mbPrimary]]
          <> [Se.Is Beam.secondaryConflictHash $ Se.Eq (Just secondary) | Just secondary <- [mbSecondary]]
  if null conflicts
    then pure []
    else
      findAllWithKV
        [ Se.And
            [ Se.Is Beam.merchantId $ Se.Eq merchant.getId,
              Se.Is Beam.permitType $ Se.Eq permitType,
              Se.Is Beam.permitDate $ Se.Eq permitDate,
              Se.Is Beam.status $ Se.In [Domain.SUBMITTED, Domain.APPROVED],
              Se.Or conflicts
            ]
        ]
