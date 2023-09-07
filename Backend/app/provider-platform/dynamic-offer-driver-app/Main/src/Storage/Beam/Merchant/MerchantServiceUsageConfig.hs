{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Storage.Beam.Merchant.MerchantServiceUsageConfig where

import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.External.AadhaarVerification.Types
import Kernel.External.Call (CallService)
import Kernel.External.Maps.Types
import Kernel.External.SMS.Types
import Kernel.External.Ticket.Types
import Kernel.External.Verification.Types
import Kernel.External.Whatsapp.Types
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common ()
import Lib.Utils ()
import Sequelize
import Tools.Beam.UtilsTH

data MerchantServiceUsageConfigT f = MerchantServiceUsageConfigT
  { merchantId :: B.C f Text,
    initiateCall :: B.C f CallService,
    getDistances :: B.C f MapsService,
    getEstimatedPickupDistances :: B.C f MapsService,
    getRoutes :: B.C f MapsService,
    getPickupRoutes :: B.C f MapsService,
    getTripRoutes :: B.C f MapsService,
    snapToRoad :: B.C f MapsService,
    getPlaceName :: B.C f MapsService,
    getPlaceDetails :: B.C f MapsService,
    autoComplete :: B.C f MapsService,
    getDistancesForCancelRide :: B.C f MapsService,
    smsProvidersPriorityList :: B.C f [SmsService],
    whatsappProvidersPriorityList :: B.C f [WhatsappService],
    verificationService :: B.C f VerificationService,
    faceVerificationService :: B.C f VerificationService,
    aadhaarVerificationService :: B.C f AadhaarVerificationService,
    issueTicketService :: B.C f IssueTicketService,
    updatedAt :: B.C f Time.UTCTime,
    createdAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table MerchantServiceUsageConfigT where
  data PrimaryKey MerchantServiceUsageConfigT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . merchantId

type MerchantServiceUsageConfig = MerchantServiceUsageConfigT Identity

$(enableKVPG ''MerchantServiceUsageConfigT ['merchantId] [])

$(mkTableInstances ''MerchantServiceUsageConfigT "merchant_service_usage_config")
