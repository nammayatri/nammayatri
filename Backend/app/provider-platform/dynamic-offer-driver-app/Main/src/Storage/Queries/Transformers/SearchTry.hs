module Storage.Queries.Transformers.SearchTry where

import Domain.Types.Common
import Kernel.Prelude

getTripCategory :: (Kernel.Prelude.Maybe Domain.Types.Common.TripCategory -> Domain.Types.Common.TripCategory)
getTripCategory tripCategory = fromMaybe (OneWay OneWayOnDemandDynamicOffer) tripCategory
