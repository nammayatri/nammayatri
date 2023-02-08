{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.DiscountTransaction where

import Data.Time (UTCTime)
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Merchant as DM
import EulerHS.Prelude hiding (id)
import Kernel.Types.Common
import Kernel.Types.Id (Id)

data DiscountTransaction = DiscountTransaction
  { bookingId :: Id DRB.Booking,
    merchantId :: Id DM.Merchant,
    discount :: Money,
    createdAt :: UTCTime
  }
  deriving (Generic)
