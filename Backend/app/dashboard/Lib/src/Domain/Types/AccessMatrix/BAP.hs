{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.AccessMatrix.BAP where

import Data.Singletons.TH
import Domain.Types.AccessMatrix.BAP.CustomerActionType as CAction
import Domain.Types.AccessMatrix.BAP.MerchantActionType as MAction
import Domain.Types.AccessMatrix.BAP.RideActionType as RAction
import Kernel.Prelude

data BAPActionType
  = CUSTOMERS CAction.CustomerActionType
  | RIDES RAction.RideActionType
  | MONITORING
  | MERCHANT MAction.MerchantActionType
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

genSingletons [''BAPActionType]
