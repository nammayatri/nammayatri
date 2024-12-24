{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Transformers.FRFSSearch where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.JourneyLeg.Types

mkJourneyLegInfo :: (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Lib.JourneyLeg.Types.JourneySearchData)
mkJourneyLegInfo agency (Just convenienceCost) (Just journeyId) (Just journeyLegOrder) pricingId (Just skipBooking) = Just $ Lib.JourneyLeg.Types.JourneySearchData {..}
mkJourneyLegInfo _ _ _ _ _ _ = Nothing
