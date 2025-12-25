{-# LANGUAGE DeriveAnyClass #-}
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

module Domain.Types (module Domain.Types, module Reexport) where

import Data.Aeson.Types
import Domain.Types.FareProductType as Reexport
import Domain.Types.ServiceTierType as Reexport
import Domain.Types.Trip as Reexport
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnumAndList)
import Kernel.Prelude
import Kernel.Storage.Esqueleto (derivePersistField)

data BknPaymentParams = BknPaymentParams
  { bankAccNumber :: Maybe Text,
    bankCode :: Maybe Text,
    vpa :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Read)

data UsageSafety = Safe | Unsafe

data GatewayAndRegistryService = ONDC | NY
  deriving (Show, Read, Eq, Ord, Generic, FromJSON, ToJSON)

$(mkBeamInstancesForEnumAndList ''GatewayAndRegistryService)
derivePersistField "GatewayAndRegistryService"
