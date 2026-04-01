{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Domain.Types.OfflineOffer where

import qualified Kernel.Beam.Lib.UtilsTH
import qualified Kernel.External.Payment.Interface.Types
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data OfflineOffer = OfflineOffer
  { createdAt :: Kernel.Prelude.UTCTime,
    discountAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    id :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.OfflineOffer.OfflineOffer,
    merchantId :: Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Text,
    offerCode :: Kernel.Prelude.Text,
    offerId :: Kernel.Prelude.Text,
    payoutAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    referenceId :: Kernel.Prelude.Text,
    status :: Kernel.External.Payment.Interface.Types.OfferState,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)
