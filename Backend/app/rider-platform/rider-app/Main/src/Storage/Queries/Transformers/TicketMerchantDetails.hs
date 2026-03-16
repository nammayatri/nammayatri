{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Transformers.TicketMerchantDetails where

import qualified Domain.Types.TicketMerchantDetails
import Kernel.Prelude

makeContactDetails :: (Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Domain.Types.TicketMerchantDetails.ContactDetails)
makeContactDetails email name number = Domain.Types.TicketMerchantDetails.ContactDetails {..}
