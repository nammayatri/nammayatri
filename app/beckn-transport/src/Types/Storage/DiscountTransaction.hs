{-# LANGUAGE UndecidableInstances #-}

module Types.Storage.DiscountTransaction where

import Beckn.Types.Amount (Amount)
import Beckn.Types.Id (Id)
import Data.Time (UTCTime)
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.Organization as Organization
import Types.Storage.ProductInstance (ProductInstance)

data DiscountTransactionT f = DiscountTransaction
  { rideBookingid :: B.C f (Id ProductInstance),
    organizationId :: B.C f (Id Organization.Organization),
    discount :: B.C f Amount,
    createdAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

type DiscountTransaction = DiscountTransactionT Identity

type DiscountTransactionPrimaryKey = B.PrimaryKey DiscountTransactionT Identity

instance B.Table DiscountTransactionT where
  data PrimaryKey DiscountTransactionT f = DiscountTransactionPrimaryKey (B.C f (Id ProductInstance))
    deriving (Generic, B.Beamable)
  primaryKey = DiscountTransactionPrimaryKey . rideBookingid

deriving instance Show DiscountTransaction

deriving instance Eq DiscountTransaction

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity DiscountTransactionT)
fieldEMod =
  B.setEntityName "discount_transaction"
    <> B.modifyTableFields
      B.tableModification
        { rideBookingid = "ride_booking_id",
          organizationId = "organization_id",
          createdAt = "created_at"
        }