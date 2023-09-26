{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Storage.Beam.Merchant.PushNotification where

import Data.Serialize
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import qualified Domain.Types.Merchant.PushNotification as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.External.Types (Language)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common ()
import Lib.Utils ()
import Sequelize
import Tools.Beam.UtilsTH

data PushNotificationT f = PushNotificationT
  { merchantId :: B.C f Text,
    pushNotificationKey :: B.C f Domain.PushNotificationKey,
    language :: B.C f Language,
    udf1 :: B.C f (Maybe Text),
    notificationSubType :: B.C f Domain.NotificationSubType,
    icon :: B.C f (Maybe Text),
    title :: B.C f Text,
    body :: B.C f Text
  }
  deriving (Generic, B.Beamable)

instance B.Table PushNotificationT where
  data PrimaryKey PushNotificationT f
    = Id (B.C f Domain.PushNotificationKey)
    deriving (Generic, B.Beamable)
  primaryKey = Id . pushNotificationKey

type PushNotification = PushNotificationT Identity

$(enableKVPG ''PushNotificationT ['merchantId, 'pushNotificationKey] [])

$(mkTableInstances ''PushNotificationT "merchant_push_notification")
