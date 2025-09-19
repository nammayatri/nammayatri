{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FinancialYearEarningsExtra where

import qualified Domain.Types.FinancialYearEarnings as DFYE
import qualified Domain.Types.Person as DP
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FinancialYearEarnings as BeamFYE
import Storage.Queries.OrphanInstances.FinancialYearEarnings

-- Extra code goes here --

findLatestByPersonIdAndFinancialYear ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DP.Person ->
  Int ->
  m (Maybe DFYE.FinancialYearEarnings)
findLatestByPersonIdAndFinancialYear personId financialYearStart =
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is BeamFYE.personId $ Se.Eq personId.getId,
          Se.Is BeamFYE.financialYearStart $ Se.Eq financialYearStart
        ]
    ]
    (Se.Desc BeamFYE.createdAt)
    (Just 1)
    Nothing
    <&> listToMaybe
