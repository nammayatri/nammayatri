{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.DiscountTransaction where

import Beckn.Types.Amount (Amount)
import Beckn.Types.Id (Id)
import Data.Time (UTCTime)
import qualified Domain.Types.Organization as DOrg
import qualified Domain.Types.RideBooking as DRB
import EulerHS.Prelude hiding (id)

data DiscountTransaction = DiscountTransaction
  { rideBookingId :: Id DRB.RideBooking,
    organizationId :: Id DOrg.Organization,
    discount :: Amount,
    createdAt :: UTCTime
  }
  deriving (Generic)
