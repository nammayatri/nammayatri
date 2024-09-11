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
import qualified Lib.JourneyPlanner.Types

mkJourneyLegInfo :: (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Lib.JourneyPlanner.Types.JourneySearchData)
mkJourneyLegInfo agency (Just convenienceCost) (Just journeyId_) (Just journeyLegOrder) (Just skipBooking) = Just $ Lib.JourneyPlanner.Types.JourneySearchData {journeyId = Kernel.Types.Id.Id journeyId_, ..}
mkJourneyLegInfo _ _ _ _ _ = Nothing
