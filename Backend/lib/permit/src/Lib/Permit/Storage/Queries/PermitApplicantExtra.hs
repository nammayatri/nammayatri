{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Permit.Storage.Queries.PermitApplicantExtra where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.Permit.Domain.Types.Common as DCommon
import qualified Lib.Permit.Domain.Types.PermitApplicant as Domain
import qualified Lib.Permit.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Permit.Storage.Beam.PermitApplicant as Beam
import Lib.Permit.Storage.Queries.OrphanInstances.PermitApplicant ()
import qualified Sequelize as Se

findByMobileHashAndMerchant :: BeamFlow.BeamFlow m r => DbHash -> Id DCommon.Merchant -> m (Maybe Domain.PermitApplicant)
findByMobileHashAndMerchant mobileHash merchant =
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.mobileNumberHash $ Se.Eq mobileHash,
          Se.Is Beam.merchantId $ Se.Eq merchant.getId
        ]
    ]
