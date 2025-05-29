{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Beam.Merchant where

import qualified Data.Time as Time
import qualified Database.Beam as B
import qualified Domain.Types.ServerName as Domain
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption (DbHash)
import Kernel.Prelude
import Kernel.Types.Beckn.City (City)

data MerchantT f = MerchantT
  { id :: B.C f Text,
    shortId :: B.C f Text,
    serverNames :: B.C f [Domain.ServerName],
    is2faMandatory :: B.C f Bool,
    defaultOperatingCity :: B.C f City,
    supportedOperatingCities :: B.C f [City],
    domain :: B.C f (Maybe Text),
    website :: B.C f (Maybe Text),
    authTokenEncrypted :: B.C f (Maybe Text),
    authTokenHash :: B.C f (Maybe DbHash),
    enabled :: B.C f (Maybe Bool),
    createdAt :: B.C f Time.UTCTime,
    requireAdminApprovalForFleetOnboarding :: B.C f (Maybe Bool),
    verifyFleetWhileLogin :: B.C f (Maybe Bool),
    hasFleetMemberHierarchy :: B.C f (Maybe Bool)
  }
  deriving (Generic, B.Beamable)

instance B.Table MerchantT where
  data PrimaryKey MerchantT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type Merchant = MerchantT Identity

$(enableKVPG ''MerchantT ['id] [])

$(mkTableInstancesGenericSchema ''MerchantT "merchant")
