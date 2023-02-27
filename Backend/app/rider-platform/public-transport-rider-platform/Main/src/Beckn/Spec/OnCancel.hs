{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Spec.OnCancel
  ( module Beckn.Spec.OnCancel,
    module Reexport,
  )
where

import Beckn.Spec.OnStatus.Descriptor as Reexport
import Beckn.Spec.OnStatus.Item as Reexport
import Beckn.Spec.OnStatus.Order as Reexport
import Beckn.Spec.OnStatus.Params as Reexport
import Beckn.Spec.OnStatus.Time as Reexport
import Kernel.Prelude

newtype OnCancelMessage = OnCancelMessage
  { order :: Order
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
