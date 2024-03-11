{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Transformers.CancellationReason where

import qualified Data.Text
import Domain.Types.CancellationReason
  ( CancellationReason (..),
    CancellationReasonCode (CancellationReasonCode),
  )
import qualified Domain.Types.CancellationReason
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, KvDbFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.CancellationReason as Beam

reasonCodeToText :: Domain.Types.CancellationReason.CancellationReasonCode -> Domain.Types.CancellationReason.CancellationReasonCode
reasonCodeToText reasonCode = Domain.Types.CancellationReason.CancellationReasonCode $ (\(CancellationReasonCode x) -> x) reasonCode
