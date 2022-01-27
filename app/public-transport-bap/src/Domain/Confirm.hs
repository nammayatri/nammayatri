module Domain.Confirm where

import Beckn.Prelude
import qualified Domain.Types.Booking as Domain
import qualified Domain.Types.Quote as Domain

data ConfirmMessageD = ConfirmMessageD
  { txnId :: Text,
    quantity :: Int,
    requestorName :: Text,
    booking :: Domain.Booking,
    quote :: Domain.Quote
  }
