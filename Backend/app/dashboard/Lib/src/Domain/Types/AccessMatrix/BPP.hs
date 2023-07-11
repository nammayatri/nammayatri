{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.AccessMatrix.BPP where

import Data.Singletons.TH
import Domain.Types.AccessMatrix.BPP.DriverActionType as DAType
import Domain.Types.AccessMatrix.BPP.DriverReferralActionType as DRAType
import Domain.Types.AccessMatrix.BPP.IssueActionType as DIAType
import Domain.Types.AccessMatrix.BPP.MerchantActionType as DMAType
import Domain.Types.AccessMatrix.BPP.MessageActionType as DMMType
import Domain.Types.AccessMatrix.BPP.RideActionType as DRType
import Domain.Types.AccessMatrix.BPP.VolunteerActionType as DVType
import Kernel.Prelude

data BPPActionType
  = DRIVERS DAType.DriverActionType
  | RIDES DRType.RideActionType
  | MONITORIING
  | MERCHANT DMAType.MerchantActionType
  | MESSAGE DMMType.MessageActionType
  | REFERRAL DRAType.DriverReferralActionType
  | ISSUE DIAType.IssueActionType
  | VOLUNTEER DVType.VolunteerActionType
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

genSingletons [''BPPActionType]
