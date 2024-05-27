{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.OnDemand.Utils.Rating where

import qualified BecknV2.OnDemand.Tags as Tag
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.Utils as Utils
import qualified Data.Text as T
import Text.Read (readMaybe)
import Prelude

getShouldFavouriteDriver :: Spec.Rating -> Maybe Bool
getShouldFavouriteDriver req = do
  let tagGroups = req.ratingTag
      tagValue = Utils.getTagV2 Tag.RATING_TAGS Tag.SHOULD_DRIVER_FAV tagGroups
   in readMaybe . T.unpack =<< tagValue
