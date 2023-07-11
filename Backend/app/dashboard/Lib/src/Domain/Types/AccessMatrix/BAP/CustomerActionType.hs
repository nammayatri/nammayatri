{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.AccessMatrix.BAP.CustomerActionType where

import Data.Singletons.TH
import Kernel.Prelude

data CustomerActionType
  = LIST
  | BLOCK
  | UNBLOCK
  | DELETE
  | INFO
  | LIST_ISSUE
  | RIDE_INFO
  | BOOKING_STATUS
  | BOOKING_LIST
  | CANCEL_BOOKING
  | CONFIRM
  | FLOW_STATUS
  | NOTIFY_EVENT
  | AUTOCOMPLETE
  | PLACE_DETAIL
  | PLACE_NAME
  | PERSON_DETAIL
  | UPDATE_PERSON
  | GET_QUOTE
  | AUTH
  | VERIFY
  | RESEND
  | LOGOUT
  | SEARCH
  | SELECT
  | SELECT_LIST
  | SELECT_RESULT
  | CANCEL_SEARCH
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

genSingletons [''CustomerActionType]
