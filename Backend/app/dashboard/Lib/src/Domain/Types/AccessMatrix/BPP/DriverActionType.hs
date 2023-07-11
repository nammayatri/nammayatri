{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.AccessMatrix.BPP.DriverActionType where

import Data.Singletons.TH
import Kernel.Prelude

data DriverActionType
  = DOCUMENTS_INFO
  | AADHAAR_INFO
  | LIST
  | ACTIVITY
  | ENABLE
  | DISABLE
  | BLOCK
  | UNBLOCK
  | LOCATION
  | INFO
  | DELETE
  | UNLINK_VEHICLE
  | END_RC_ASSOCIATION
  | UNLINK_DL
  | UPDATE_PHONE_NUMBER
  | ADD_VEHICLE
  | UPDATE_DRIVER_NAME
  | CLEAR_ON_RIDE_STUCK_DRIVER_IDS
  | DOCUMENT_LIST
  | GET_DOCUMENT
  | UPLOAD_DOCUMENT
  | REGISTER_DL
  | REGISTER_RC
  | GENERATE_AADHAAR_OTP
  | VERIFY_AADHAAR_OTP
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

genSingletons [''DriverActionType]
