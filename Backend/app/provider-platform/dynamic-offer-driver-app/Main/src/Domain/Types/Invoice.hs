module Domain.Types.Invoice where

import Data.Aeson
import qualified Domain.Types.DriverFee as DF
import Kernel.Prelude
import Kernel.Types.Id

data Invoice = Invoice
  { id :: Id Invoice,
    invoiceShortId :: Text,
    driverFeeId :: Id DF.DriverFee,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)
