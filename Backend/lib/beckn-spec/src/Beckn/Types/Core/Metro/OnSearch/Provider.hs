 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
 
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program 
 
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of 
 
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Metro.OnSearch.Provider where

import Beckn.Types.Core.Metro.OnSearch.Descriptor (Descriptor)
import Beckn.Types.Core.Metro.OnSearch.Fulfillment
import Beckn.Types.Core.Metro.OnSearch.Item
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (exp, id)

data Provider = Provider
  { id :: Text,
    descriptor :: Descriptor,
    items :: [Item],
    fulfillments :: [Fulfillment]
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)
