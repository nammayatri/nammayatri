{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Lib.Payment.Domain.Types.PayoutTransaction where
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Lib.Payment.Domain.Types.PayoutOrder
import qualified Kernel.Beam.Lib.UtilsTH
import qualified Tools.Beam.UtilsTH



data PayoutTransaction
    = PayoutTransaction {amount :: Kernel.Types.Common.Price,
                         beneficiaryAccount :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                         beneficiaryIfsc :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                         beneficiaryName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                         createdAt :: Kernel.Prelude.UTCTime,
                         fulfillmentMethod :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                         gateWayRefId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                         id :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutTransaction.PayoutTransaction,
                         merchantId :: Kernel.Prelude.Text,
                         merchantOperatingCityId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                         payoutOrderId :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutOrder.PayoutOrder,
                         status :: Kernel.Prelude.Text,
                         transactionRef :: Kernel.Prelude.Text,
                         updatedAt :: Kernel.Prelude.UTCTime}
    deriving Generic



