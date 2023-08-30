{-# LANGUAGE DerivingStrategies #-}
{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.BlackListOrg where

import Data.Aeson
import Domain.Types.Common
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Types.Registry (Subscriber)
import Kernel.Utils.TH (mkFromHttpInstanceForEnum)
import Tools.Beam.UtilsTH (mkBeamInstancesForEnum)

data BlackListOrgType
  = PROVIDER
  | APP
  | GATEWAY
  deriving stock (Show, Eq, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

$(mkBeamInstancesForEnum ''BlackListOrgType)

$(mkFromHttpInstanceForEnum ''BlackListOrgType)

data BlackListOrgD (s :: UsageSafety) = BlackListOrg
  { id :: Id BlackListOrg,
    subscriberId :: ShortId Subscriber,
    _type :: BlackListOrgType
  }
  deriving (Generic)

type BlackListOrg = BlackListOrgD 'Safe

instance FromJSON (BlackListOrgD 'Unsafe)

instance ToJSON (BlackListOrgD 'Unsafe)
