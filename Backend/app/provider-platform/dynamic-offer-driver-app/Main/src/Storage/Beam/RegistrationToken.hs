{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Storage.Beam.RegistrationToken where

import qualified Database.Beam as B
import qualified Domain.Types.RegistrationToken as Domain
import Tools.Beam.UtilsTH
import Kernel.Prelude
import Kernel.Types.Common hiding (id)

data RegistrationTokenT f = RegistrationTokenT
  { id :: B.C f Text,
    token :: B.C f RegToken,
    attempts :: B.C f Int,
    authMedium :: B.C f Domain.Medium,
    authType :: B.C f Domain.LoginType,
    authValueHash :: B.C f Text,
    verified :: B.C f Bool,
    authExpiry :: B.C f Int,
    tokenExpiry :: B.C f Int,
    entityId :: B.C f Text,
    merchantId :: B.C f Text,
    entityType :: B.C f Domain.RTEntityType,
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime,
    info :: B.C f (Maybe Text),
    alternateNumberAttempts :: B.C f Int
  }
  deriving (Generic, B.Beamable)

instance B.Table RegistrationTokenT where
  data PrimaryKey RegistrationTokenT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type RegistrationToken = RegistrationTokenT Identity

$(enableKVPG ''RegistrationTokenT ['id] [['token], ['entityId]])

$(mkTableInstances ''RegistrationTokenT "registration_token")
