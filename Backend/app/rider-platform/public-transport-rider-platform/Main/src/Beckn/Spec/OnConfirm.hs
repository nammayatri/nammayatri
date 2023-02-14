 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
 
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program 
 
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of 
 
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Spec.OnConfirm (module Beckn.Spec.OnConfirm, module Reexport) where

import Beckn.Spec.OnConfirm.Descriptor as Reexport
import Beckn.Spec.OnConfirm.Item as Reexport
import Beckn.Spec.OnConfirm.Order as Reexport
import Beckn.Spec.OnConfirm.Params as Reexport
import Beckn.Spec.OnConfirm.Quantity as Reexport
import Beckn.Spec.OnConfirm.Time as Reexport
import Kernel.Prelude
import Kernel.Utils.GenericPretty

newtype OnConfirmMessage = OnConfirmMessage
  { order :: Order
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, PrettyShow, ToSchema)
