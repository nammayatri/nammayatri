{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.DiscountTransaction where

import Beckn.Types.Amount (Amount)
import Beckn.Types.Id (Id)
import Data.Time (UTCTime)
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Organization as DOrg
import EulerHS.Prelude hiding (id)

data DiscountTransaction = DiscountTransaction
  { bookingId :: Id DRB.Booking,
    organizationId :: Id DOrg.Organization,
    discount :: Amount,
    createdAt :: UTCTime
  }
  deriving (Generic)
