{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Transformers.FRFSSearch where

import Kernel.Prelude
import qualified Lib.JourneyPlannerTypes

mkJourneyLegInfo :: (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Lib.JourneyPlannerTypes.JourneySearchData)
mkJourneyLegInfo agency (Just convenienceCost) (Just journeyId) (Just journeyLegOrder) pricingId (Just skipBooking) = Just $ Lib.JourneyPlannerTypes.JourneySearchData {..}
mkJourneyLegInfo _ _ _ _ _ _ = Nothing
