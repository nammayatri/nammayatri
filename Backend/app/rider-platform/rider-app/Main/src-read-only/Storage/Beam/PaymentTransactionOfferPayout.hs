{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.PaymentTransactionOfferPayout where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Types.Common
import qualified Kernel.Prelude
import qualified Database.Beam as B



data PaymentTransactionOfferPayoutT f
    = PaymentTransactionOfferPayoutT {amountPaidByUser :: (B.C f Kernel.Types.Common.HighPrecMoney),
                                      bookingCreatedAt :: (B.C f Kernel.Prelude.UTCTime),
                                      createdAt :: (B.C f Kernel.Prelude.UTCTime),
                                      id :: (B.C f Kernel.Prelude.Text),
                                      isConsumedByCron :: (B.C f Kernel.Prelude.Bool),
                                      merchantId :: (B.C f Kernel.Prelude.Text),
                                      merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
                                      networkOrderId :: (B.C f Kernel.Prelude.Text),
                                      offerCode :: (B.C f Kernel.Prelude.Text),
                                      offerId :: (B.C f Kernel.Prelude.Text),
                                      orderAmount :: (B.C f Kernel.Types.Common.HighPrecMoney),
                                      paymentRRN :: (B.C f Kernel.Prelude.Text),
                                      paymentTxnId :: (B.C f Kernel.Prelude.Text),
                                      providerAgency :: (B.C f Kernel.Prelude.Text),
                                      settledAmount :: (B.C f Kernel.Types.Common.HighPrecMoney),
                                      settlementReference :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                                      settlementTs :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
                                      updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table PaymentTransactionOfferPayoutT
    where data PrimaryKey PaymentTransactionOfferPayoutT f = PaymentTransactionOfferPayoutId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = PaymentTransactionOfferPayoutId . id
type PaymentTransactionOfferPayout = PaymentTransactionOfferPayoutT Identity

$(enableKVPG (''PaymentTransactionOfferPayoutT) [('id)] [[('networkOrderId)]])

$(mkTableInstances (''PaymentTransactionOfferPayoutT) "payment_transaction_offer_payout")

