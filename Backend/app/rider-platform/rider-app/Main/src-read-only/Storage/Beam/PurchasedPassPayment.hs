{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.PurchasedPassPayment where

import qualified Data.Time.Calendar
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.PurchasedPass
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data PurchasedPassPaymentT f = PurchasedPassPaymentT
  { amount :: (B.C f Kernel.Types.Common.HighPrecMoney),
    endDate :: (B.C f Data.Time.Calendar.Day),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    orderId :: (B.C f Kernel.Prelude.Text),
    passCode :: (B.C f Kernel.Prelude.Text),
    passName :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    personId :: (B.C f Kernel.Prelude.Text),
    profilePicture :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    purchasedPassId :: (B.C f Kernel.Prelude.Text),
    startDate :: (B.C f Data.Time.Calendar.Day),
    status :: (B.C f Domain.Types.PurchasedPass.StatusType),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table PurchasedPassPaymentT where
  data PrimaryKey PurchasedPassPaymentT f = PurchasedPassPaymentId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = PurchasedPassPaymentId . id

type PurchasedPassPayment = PurchasedPassPaymentT Identity

$(enableKVPG (''PurchasedPassPaymentT) [('id)] [[('orderId)], [('purchasedPassId)]])

$(mkTableInstances (''PurchasedPassPaymentT) "purchased_pass_payment")
