{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Transformers.TicketMerchantDetails where

import qualified Domain.Types.TicketMerchantDetails
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)

makeContactDetails :: (Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Domain.Types.TicketMerchantDetails.ContactDetails)
makeContactDetails email name number = Domain.Types.TicketMerchantDetails.ContactDetails {..}
