module BecknV2.FRFS.Utils where

import qualified BecknV2.FRFS.Types as Spec
import qualified Data.Text as T
import Kernel.Prelude
import Kernel.Types.Common

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
