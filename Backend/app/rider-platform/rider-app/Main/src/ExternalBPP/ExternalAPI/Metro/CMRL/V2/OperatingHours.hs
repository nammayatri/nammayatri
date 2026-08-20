module ExternalBPP.ExternalAPI.Metro.CMRL.V2.OperatingHours where

import qualified Data.Text as T
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Data.Time.LocalTime (TimeZone (..), ZonedTime (..), zonedTimeToUTC)
import Domain.Types.Extra.IntegratedBPPConfig
import qualified ExternalBPP.ExternalAPI.Metro.CMRL.V2.BusinessHour as BusinessHour
import Kernel.External.MasterCloudForward (HasMasterCloudForwarder)
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.App
import Kernel.Utils.Common

businessDateTag, operationHoursStartTag, operationHoursEndTag :: Text
businessDateTag = "BUSINESS_DATE"
operationHoursStartTag = "OPERATION_HOURS_START_TIME"
operationHoursEndTag = "OPERATION_HOURS_END_TIME"

operatingHoursTagNames :: [(Text, Text)]
operatingHoursTagNames =
  [ (businessDateTag, businessDateTag),
    (operationHoursStartTag, operationHoursStartTag),
    (operationHoursEndTag, operationHoursEndTag),
    ("TICKET_SELLING_START_TIME", "TICKET_SELLING_START_TIME"),
    ("TICKET_SELLING_END_TIME", "TICKET_SELLING_END_TIME"),
    ("TICKET_BOOKING_RESTRICTION_START_TIME", "TICKET BOOKING RESTRICTION START TIME"),
    ("TICKET_BOOKING_RESTRICTION_END_TIME", "TICKET BOOKING RESTRICTION END TIME")
  ]

operatorTimeFormat :: String
operatorTimeFormat = "%d-%m-%Y %H:%M:%S"

publishedTimeFormat :: String
publishedTimeFormat = "%Y-%m-%dT%H:%M:%S%Ez"

istTimeZone :: TimeZone
istTimeZone = TimeZone (5 * 60 + 30) False "IST"

getOperatingHoursTags :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r, HasRequestId r, MonadReader r m, HasMasterCloudForwarder r) => CMRLV2Config -> m [(Text, Text)]
getOperatingHoursTags config = do
  response <- BusinessHour.getBusinessHour config
  let tags = tagsFromParams response.commonParamList
  logInfo $ "[CMRLV2:BusinessHours] Publishing " <> show (length tags) <> " of " <> show (length operatingHoursTagNames) <> " operating-hours tags"
  return tags

tagsFromParams :: [BusinessHour.CommonParam] -> [(Text, Text)]
tagsFromParams params = mapMaybe publishedTag operatingHoursTagNames
  where
    publishedTag (tagCode, apiParamName) = do
      param <- find (\p -> p.paramName == apiParamName) params
      istTime <- parseTimeM True defaultTimeLocale operatorTimeFormat (T.unpack param.paramValue) :: Maybe LocalTime
      return (tagCode, renderTagValue tagCode istTime)

renderTagValue :: Text -> LocalTime -> Text
renderTagValue tagCode istTime
  | tagCode == businessDateTag = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d" istTime
  | otherwise = T.pack $ formatTime defaultTimeLocale publishedTimeFormat (ZonedTime istTime istTimeZone)

getOperatingWindow :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r, HasRequestId r, MonadReader r m, HasMasterCloudForwarder r) => CMRLV2Config -> m (Maybe (UTCTime, UTCTime))
getOperatingWindow config = operatingWindowFromTags <$> getOperatingHoursTags config

operatingWindowFromTags :: [(Text, Text)] -> Maybe (UTCTime, UTCTime)
operatingWindowFromTags tags = do
  startTime <- parsePublished =<< lookup operationHoursStartTag tags
  endTime <- parsePublished =<< lookup operationHoursEndTag tags
  return (startTime, endTime)
  where
    parsePublished value = zonedTimeToUTC <$> (parseTimeM True defaultTimeLocale publishedTimeFormat (T.unpack value) :: Maybe ZonedTime)
