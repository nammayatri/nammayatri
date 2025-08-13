module Storage.Queries.Transformers.FRFSSearch where

import Kernel.Prelude
import qualified Lib.JourneyLeg.Types

mkJourneyLegInfo :: Maybe Text -> Maybe Int -> Maybe Bool -> Maybe Bool -> Maybe Text -> Maybe Bool -> Maybe Lib.JourneyLeg.Types.JourneySearchData
mkJourneyLegInfo agency (Just convenienceCost) isDeleted onSearchFailed pricingId (Just skipBooking) = Just $ Lib.JourneyLeg.Types.JourneySearchData {..}
mkJourneyLegInfo _ _ _ _ _ _ = Nothing
