{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Domain.Types.PersonOfferStats where

import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Lib.Payment.Domain.Types.Offer
import qualified Tools.Beam.UtilsTH

data PersonOfferStats = PersonOfferStats
  { createdAt :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.PersonOfferStats.PersonOfferStats,
    offerAppliedCount :: Kernel.Prelude.Int,
    offerId :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.Offer.Offer,
    personId :: Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, (Show), (ToJSON), (FromJSON))
