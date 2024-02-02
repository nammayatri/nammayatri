{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}


module Common.RemoteConfig.Types where

import Prelude
import Data.Maybe (Maybe)
import Foreign.Class (class Decode, decode)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Presto.Core.Utils.Encoding (defaultDecode)

type RemoteConfig a = {
    bangalore :: a,
    kolkata :: a
}

newtype RCCarousel = RCCarousel {
    text_color:: String,
    text:: String,
    cta_text:: String,
    cta_action:: Maybe String,
    cta_link:: String,
    cta_icon:: String,
    banner_color:: String,
    banner_image:: String,
    cta_background_color:: String,
    cta_text_color:: String,
    cta_corner_radius:: String,
    cta_image_url:: String
}

derive instance genericRCCarousel :: Generic RCCarousel _
instance decodeRCCarousel :: Decode RCCarousel where decode = defaultDecode
