{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.AccessMatrix.BPP.MerchantActionType where

import Data.Singletons.TH
import Kernel.Prelude

data MerchantActionType
  = UPDATE
  | COMMON_CONFIG
  | COMMON_CONFIG_UPDATE
  | DRIVER_POOL_CONFIG
  | DRIVER_POOL_CONFIG_UPDATE
  | DRIVER_POOL_CONFIG_CREATE
  | DRIVER_INTELLIGENT_POOL_CONFIG
  | DRIVER_INTELLIGENT_POOL_CONFIG_UPDATE
  | ONBOARDING_DOCUMENT_CONFIG
  | ONBOARDING_DOCUMENT_CONFIG_UPDATE
  | ONBOARDING_DOCUMENT_CONFIG_CREATE
  | SERVICE_USAGE_CONFIG
  | MAPS_SERVICE_CONFIG_UPDATE
  | MAPS_SERVICE_USAGE_CONFIG_UPDATE
  | SMS_SERVICE_CONFIG_UPDATE
  | SMS_SERVICE_USAGE_CONFIG_UPDATE
  | VERIFICATION_SERVICE_CONFIG_UPDATE
  | CREATE_FP_DRIVER_EXTRA_FEE
  | UPDATE_FP_DRIVER_EXTRA_FEE
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

genSingletons [''MerchantActionType]
