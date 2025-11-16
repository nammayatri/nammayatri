module Domain.Types.DriverFlowStatus where

import Data.Aeson
import qualified Data.Text as T
import Kernel.Prelude
import qualified Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Utils.TH
import Tools.Beam.UtilsTH

data DriverFlowStatus = ONLINE | OFFLINE | SILENT | ON_PICKUP | ON_RIDE | ACTIVE | INACTIVE
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

instance CH.ClickhouseValue DriverFlowStatus

$(mkHttpInstancesForEnum ''DriverFlowStatus)

$(mkBeamInstancesForEnumAndList ''DriverFlowStatus)

getStatusKey :: Text -> DriverFlowStatus -> Text
getStatusKey entityId status = "driver_status:" <> (T.pack . show) status <> ":" <> entityId

statusList :: [DriverFlowStatus]
statusList = [ONLINE, OFFLINE, SILENT, ON_PICKUP, ON_RIDE, ACTIVE, INACTIVE]

allKeys :: Text -> [Text]
allKeys entityId = map (getStatusKey entityId) statusList

-- Returns the current ONLINE count/value for the given entity (e.g., fleet owner) from Redis
getOnlineKeyValue :: (Redis.HedisFlow m r) => Text -> m (Maybe Int)
getOnlineKeyValue entityId = do
  let key = getStatusKey entityId ONLINE
  Redis.get @Int key
