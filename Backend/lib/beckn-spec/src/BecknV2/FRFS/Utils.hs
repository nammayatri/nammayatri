module BecknV2.FRFS.Utils where

import qualified BecknV2.FRFS.Enums as Spec
import qualified BecknV2.FRFS.Types as Spec hiding (Domain)
import qualified BecknV2.OnDemand.Enums as BecknSpec
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import EulerHS.Prelude (ByteString)
import Kernel.Prelude
import Kernel.Types.Common
import qualified Kernel.Types.Error as Error
import Kernel.Utils.Error

tfDescriptor :: Maybe Text -> Maybe Text -> Maybe Spec.Descriptor
tfDescriptor mCode mName = do
  code <- mCode
  name <- mName
  return
    Spec.Descriptor
      { descriptorCode = Just $ code,
        descriptorImages = Nothing,
        descriptorName = Just $ name
      }

parsePrice :: Spec.Price -> Maybe Price
parsePrice specPrice = do
  currency <- parseCurrency specPrice
  money <- parseMoney specPrice
  Just $ mkPrice (Just currency) money

parseMoney :: Spec.Price -> Maybe HighPrecMoney
parseMoney price =
  price.priceValue >>= (readMaybe . T.unpack)

parseOfferPrice :: Spec.Price -> Maybe Price
parseOfferPrice specPrice = do
  currency <- parseCurrency specPrice
  offerMoney <- parseOfferMoney specPrice
  Just $ mkPrice (Just currency) offerMoney

parseOfferMoney :: Spec.Price -> Maybe HighPrecMoney
parseOfferMoney price =
  price.priceOfferedValue >>= (readMaybe . T.unpack)

-- TODO check what we receive from bpp
parseCurrency :: Spec.Price -> Maybe Currency
parseCurrency price =
  price.priceCurrency >>= (readMaybe . T.unpack)

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

durationToText :: NominalDiffTime -> Text
durationToText duration = T.pack $ iso8601Show $ calendarTimeTime duration

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
  tagGroupList <- tagGroup.tagGroupList
  tag <- find (\tag -> descriptorCode tag.tagDescriptor == Just tagCode) tagGroupList
  tag.tagValue
  where
    descriptorCode :: Maybe Spec.Descriptor -> Maybe Text
    descriptorCode = (>>= (.descriptorCode))

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
  unless (version == supportedVersion || version == "2.0.1") $
    throwError Error.UnsupportedCoreVer

frfsVehicleCategoryToBecknVehicleCategory :: Spec.VehicleCategory -> BecknSpec.VehicleCategory
frfsVehicleCategoryToBecknVehicleCategory = \case
  Spec.BUS -> BecknSpec.BUS
  Spec.METRO -> BecknSpec.METRO
  Spec.SUBWAY -> BecknSpec.SUBWAY

becknVehicleCategoryToFrfsVehicleCategory :: BecknSpec.VehicleCategory -> Spec.VehicleCategory
becknVehicleCategoryToFrfsVehicleCategory = \case
  BecknSpec.BUS -> Spec.BUS
  BecknSpec.METRO -> Spec.METRO
  BecknSpec.SUBWAY -> Spec.SUBWAY
  _ -> Spec.METRO

getAndValidateCancellationParams :: [Spec.QuotationBreakupInner] -> Spec.OrderStatus -> Either Text (HighPrecMoney, Maybe HighPrecMoney, Maybe HighPrecMoney)
getAndValidateCancellationParams quoteBreakup _ = do
  baseFare <- findCancellationParams Spec.BASE_FARE & maybe (Left "CancellationParams baseFare not found") Right
  let refundAmount = findCancellationParams Spec.REFUND
      cancellationCharges = findCancellationParams Spec.CANCELLATION_CHARGES
  Right (baseFare, refundAmount, cancellationCharges)
  where
    findCancellationParams :: Spec.CancellationParams -> Maybe HighPrecMoney
    findCancellationParams titleToFind =
      find (\qb -> qb.quotationBreakupInnerTitle == Just (show titleToFind)) quoteBreakup >>= (.quotationBreakupInnerPrice) >>= parseMoney

-- use below with care with care
defaultBusBoardingRelationshitCfg :: Spec.ServiceTierType -> [Spec.ServiceTierType]
defaultBusBoardingRelationshitCfg Spec.ORDINARY = [Spec.ORDINARY]
defaultBusBoardingRelationshitCfg Spec.EXECUTIVE = [Spec.ORDINARY, Spec.EXECUTIVE]
defaultBusBoardingRelationshitCfg Spec.AC = [Spec.ORDINARY, Spec.EXECUTIVE, Spec.AC]
defaultBusBoardingRelationshitCfg a = [a]

discoverySearchCounterKey :: Text -> Text
discoverySearchCounterKey transactionId = "FRFS:Discovery:SearchReq:Counter" <> transactionId

unescapeQuotedJSON :: ByteString -> Maybe ByteString
unescapeQuotedJSON bs =
  if BS.length bs >= 2 && BS.head bs == 34 && BS.last bs == 34
    then
      let inner = BS.init (BS.tail bs)
       in case A.eitherDecodeStrict' ("\"" <> inner <> "\"") :: Either String T.Text of
            Right txt -> Just (TE.encodeUtf8 txt)
            Left _ -> Nothing
    else Nothing
