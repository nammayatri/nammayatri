{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.Types (module Domain.Types, module Reexport) where

import Domain.Types.FareProductType as Reexport
import Domain.Types.ServiceTierType as Reexport
import Domain.Types.Trip as Reexport
import Kernel.Prelude

data BknPaymentParams = BknPaymentParams
  { bankAccNumber :: Maybe Text,
    bankCode :: Maybe Text,
    vpa :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Read)

data UsageSafety = Safe | Unsafe
