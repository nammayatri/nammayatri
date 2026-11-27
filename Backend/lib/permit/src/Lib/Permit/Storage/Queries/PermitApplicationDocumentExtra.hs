{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Permit.Storage.Queries.PermitApplicationDocumentExtra where

import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.Permit.Domain.Types.PermitApplication as DApplication
import Lib.Permit.Domain.Types.PermitApplicationDocument (PermitDocumentType)
import qualified Lib.Permit.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Permit.Storage.Beam.PermitApplicationDocument as Beam
import Lib.Permit.Storage.Queries.OrphanInstances.PermitApplicationDocument ()
import qualified Sequelize as Se

deleteByApplicationIdAndTypes :: BeamFlow.BeamFlow m r => Id DApplication.PermitApplication -> [PermitDocumentType] -> m ()
deleteByApplicationIdAndTypes applicationId documentTypes =
  deleteWithKV
    [ Se.And
        [ Se.Is Beam.applicationId $ Se.Eq applicationId.getId,
          Se.Is Beam.documentType $ Se.In documentTypes
        ]
    ]
