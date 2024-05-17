{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Transformers.SearchTry where

import Domain.Types.Common
import qualified Domain.Types.Common
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)

getTripCategory :: (Kernel.Prelude.Maybe Domain.Types.Common.TripCategory -> Domain.Types.Common.TripCategory)
getTripCategory tripCategory = fromMaybe (OneWay OneWayOnDemandDynamicOffer) tripCategory
