module Storage.Queries.Transformers.CancellationReason where

import Domain.Types.CancellationReason
  ( CancellationReasonCode (CancellationReasonCode),
  )
import Kernel.Prelude

reasonCodeToText :: Domain.Types.CancellationReason.CancellationReasonCode -> Domain.Types.CancellationReason.CancellationReasonCode
reasonCodeToText reasonCode = Domain.Types.CancellationReason.CancellationReasonCode $ (\(CancellationReasonCode x) -> x) reasonCode
