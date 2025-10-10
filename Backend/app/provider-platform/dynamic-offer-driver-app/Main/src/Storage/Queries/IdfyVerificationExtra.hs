{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.IdfyVerificationExtra where

import qualified Domain.Types.DocumentVerificationConfig as DVC
import qualified Domain.Types.IdfyVerification as DIdfy
import qualified Domain.Types.Person as DP
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.IdfyVerification as Beam
import Storage.Queries.IdfyVerification ()

findLatestByDriverIdAndDocType ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Int ->
  Int ->
  Id DP.Person ->
  DVC.DocumentType ->
  UTCTime ->
  UTCTime ->
  m ([DIdfy.IdfyVerification])
findLatestByDriverIdAndDocType limit offset driverId docType fromDate toDate = do
  findAllWithOptionsKV
    [ Se.And
        ( [Se.Is Beam.driverId $ Se.Eq (getId driverId)]
            <> [Se.Is Beam.docType $ Se.Eq docType]
            <> [Se.Is Beam.createdAt $ Se.GreaterThanOrEq fromDate]
            <> [Se.Is Beam.createdAt $ Se.LessThanOrEq toDate]
        )
    ]
    (Se.Desc Beam.createdAt)
    (Just limit)
    (Just offset)
