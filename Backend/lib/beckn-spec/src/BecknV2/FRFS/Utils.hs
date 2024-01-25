module BecknV2.FRFS.Utils where

import qualified BecknV2.FRFS.Enums as Spec
import qualified BecknV2.FRFS.Types as Spec hiding (Domain)
import qualified Data.Aeson as A
import qualified Data.Text as T
import Data.Time
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Kernel.Prelude
import Kernel.Types.Common
import qualified Kernel.Types.Error as Error
import Kernel.Utils.Error

tfDescriptor :: Maybe Text -> Maybe Text -> Maybe Spec.Descriptor
tfDescriptor mCode mName = do
  name <- mCode
  code <- mName
  return
    Spec.Descriptor
      { descriptorCode = Just $ code,
        descriptorImages = Nothing,
        descriptorName = Just $ name
      }

parseMoney :: Spec.Price -> Maybe HighPrecMoney
parseMoney price =
  price.priceValue >>= (readMaybe . T.unpack)

getQuoteValidTill :: UTCTime -> Text -> Maybe UTCTime
getQuoteValidTill contextTime time = do
  valid <- parseISO8601Duration time
  Just $ addDurationToUTCTime contextTime valid

-- Parse ISO8601 duration and return the number of seconds
parseISO8601Duration :: Text -> Maybe NominalDiffTime
parseISO8601Duration durationStr = do
  (calenderDiffernceTime :: CalendarDiffTime) <- iso8601ParseM $ T.unpack durationStr
  Just $ ctTime calenderDiffernceTime

-- Add the parsed duration to a given UTCTime
addDurationToUTCTime :: UTCTime -> NominalDiffTime -> UTCTime
addDurationToUTCTime time duration = addUTCTime duration time

ack :: Spec.AckResponse
ack =
  Spec.AckResponse
    { ackResponseError = Nothing,
      ackResponseMessage =
        Spec.AckMessage
          { ackMessageAck =
              Spec.Ack
                { ackStatus = Just "200",
                  ackTags = Nothing
                }
          }
    }

nack :: Text -> Text -> Spec.AckResponse
nack errorCode errorMessage =
  Spec.AckResponse
    { ackResponseError =
        Just $
          Spec.Error
            { errorCode = Just errorCode,
              errorMessage = Just errorMessage,
              errorPaths = Nothing
            },
      ackResponseMessage =
        Spec.AckMessage
          { ackMessageAck =
              Spec.Ack
                { ackStatus = Just errorCode,
                  ackTags = Nothing
                }
          }
    }

type TagGroupCode = Text

type TagCode = Text

getTag :: TagGroupCode -> TagCode -> [Spec.TagGroup] -> Maybe Text
getTag tagGroupCode tagCode tagGroups = do
  tagGroup <- find (\tagGroup -> descriptorCode tagGroup.tagGroupDescriptor == Just tagGroupCode) tagGroups
  case tagGroup.tagGroupList of
    Nothing -> Nothing
    Just tagGroupList -> do
      tag <- find (\tag -> descriptorCode tag.tagDescriptor == Just tagCode) tagGroupList
      tag.tagValue
  where
    descriptorCode :: Maybe Spec.Descriptor -> Maybe Text
    descriptorCode (Just desc) = desc.descriptorCode
    descriptorCode Nothing = Nothing

type Lat = Double

type Lon = Double

parseGPS :: Text -> Maybe (Lat, Lon)
parseGPS gps = do
  let (lat, lon) = T.breakOn "," gps
  lat' <- readMaybe $ T.unpack lat
  lon' <- readMaybe $ T.unpack $ T.drop 1 lon
  return (lat', lon')

getUTCTime :: Text -> Maybe UTCTime
getUTCTime txt = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" (T.unpack txt)

validateContext :: MonadFlow m => Spec.Action -> Spec.Context -> m ()
validateContext action context = do
  validateDomain Spec.FRFS context
  validateContextCommons action context

validateDomain :: MonadFlow m => Spec.Domain -> Spec.Context -> m ()
validateDomain expectedDomain context = do
  domainText <- context.contextDomain & fromMaybeM (Error.InvalidRequest "Missing contextDomain")
  domain <- A.decode (A.encode domainText) & fromMaybeM (Error.InvalidRequest $ "Error in parsing contextDomain: " <> domainText)
  unless (domain == expectedDomain) $
    throwError Error.InvalidDomain

validateContextCommons :: MonadFlow m => Spec.Action -> Spec.Context -> m ()
validateContextCommons expectedAction context = do
  validateAction expectedAction context
  validateCoreVersion context

validateAction :: MonadFlow m => Spec.Action -> Spec.Context -> m ()
validateAction expectedAction context = do
  actionText <- context.contextAction & fromMaybeM (Error.InvalidRequest "Missing contextAction")
  action <- A.decode (A.encode actionText) & fromMaybeM (Error.InvalidRequest $ "Error in parsing contextAction: " <> actionText)
  unless (action == expectedAction) $
    throwError Error.InvalidAction

validateCoreVersion :: MonadFlow m => Spec.Context -> m ()
validateCoreVersion context = do
  let supportedVersion = "2.0.0"
  version <- context.contextVersion & fromMaybeM (Error.InvalidRequest "Missing contextVersion")
  unless (version == supportedVersion) $
    throwError Error.UnsupportedCoreVer
