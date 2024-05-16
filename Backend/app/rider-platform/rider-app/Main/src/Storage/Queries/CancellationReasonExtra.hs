{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.CancellationReasonExtra where

import Domain.Types.CancellationReason
import qualified Domain.Types.CancellationReason as Domain
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.CancellationReason as BeamCR
import Storage.Queries.OrphanInstances.CancellationReason

-- Extra code goes here --

-- Not querying by Id. In case the table is enabled someday, better to route this through DB
findAll :: KvDbFlow m r => CancellationStage -> m [CancellationReason]
findAll cancStage = do
  seCaseCondition <- case cancStage of
    OnSearch -> pure $ Se.Is BeamCR.onSearch $ Se.Eq True
    OnInit -> pure $ Se.Is BeamCR.onInit $ Se.Eq True
    OnConfirm -> pure $ Se.Is BeamCR.onConfirm $ Se.Eq True
    OnAssign -> pure $ Se.Is BeamCR.onAssign $ Se.Eq True
  findAllWithOptionsDb [Se.And [Se.Is BeamCR.enabled $ Se.Eq True, seCaseCondition]] (Se.Desc BeamCR.priority) Nothing Nothing
