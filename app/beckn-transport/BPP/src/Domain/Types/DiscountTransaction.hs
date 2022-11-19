{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.DiscountTransaction where

import Beckn.Types.Common
import Beckn.Types.Id (Id)
import Data.Time (UTCTime)
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Merchant as DM
import EulerHS.Prelude hiding (id)

data DiscountTransaction = DiscountTransaction
  { bookingId :: Id DRB.Booking,
    merchantId :: Id DM.Merchant,
    discount :: Money,
    createdAt :: UTCTime
  }
  deriving (Generic)
