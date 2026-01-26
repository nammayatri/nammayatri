{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Domain.Types.TransferTransaction (module Lib.Payment.Domain.Types.TransferTransaction, module ReExport) where

import qualified Data.Aeson
import qualified Kernel.Beam.Lib.UtilsTH
import qualified Kernel.External.Payment.Stripe.Types.Transfer
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Lib.Payment.Domain.Types.Common
import Lib.Payment.Domain.Types.Extra.TransferTransaction as ReExport
import qualified Lib.Payment.Domain.Types.Extra.TransferTransaction

data TransferTransaction = TransferTransaction
  { amount :: Kernel.Types.Common.HighPrecMoney,
    currency :: Kernel.Types.Common.Currency,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    destinationAccountId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    entityId :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.Extra.TransferTransaction.TransferEntity,
    entityName :: Lib.Payment.Domain.Types.Extra.TransferTransaction.TransferEntityName,
    errorCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    errorMessage :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.TransferTransaction.TransferTransaction,
    idAssignedByServiceProvider :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    isApiCallSuccess :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    merchantId :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.Common.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.Common.MerchantOperatingCity,
    responseDump :: Kernel.Prelude.Maybe Data.Aeson.Value,
    senderAccountId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    status :: Kernel.Prelude.Maybe Kernel.External.Payment.Stripe.Types.Transfer.TransferStatus,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)
