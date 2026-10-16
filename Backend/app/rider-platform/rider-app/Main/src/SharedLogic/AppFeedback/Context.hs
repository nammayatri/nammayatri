-- | The JSON object the APP-FEEDBACK targeting logic, a question's subjectCondition and its action
-- rules are evaluated against. Fields are listed explicitly (no whole-entity serialisation) so phone
-- numbers and other PII never reach a rule or the dashboard preview.
module SharedLogic.AppFeedback.Context
  ( AppFeedbackContext (..),
    RiderCtx (..),
    SubjectCtx (..),
    TicketCtx (..),
    JourneyCtx (..),
    HistoryCtx (..),
    DerivedCtx (..),
    Subject (..),
    loadSubject,
    buildContext,
    withAnswer,
  )
where

import Data.Default.Class (Default (..))
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Time as Time
import Domain.Types.AppFeedbackConfig (AppFeedbackPlacement (..))
import qualified Domain.Types.AppFeedbackResponse as DAFR
import qualified Domain.Types.FRFSTicketBooking as DFTB
import qualified Domain.Types.Journey as DJ
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Feedback.Context (AnswerCtx (..), mkAnswerCtx)
import Lib.Feedback.Rule (stableBucket)
import qualified Lib.Feedback.Types as LFT
import qualified Storage.Queries.FRFSTicketBooking as QFTB
import qualified Storage.Queries.Journey as QJourney
import Tools.Error

data AppFeedbackContext = AppFeedbackContext
  { -- | Nothing while building the manifest (the question may be shown at any of its placements).
    placement :: Maybe AppFeedbackPlacement,
    rider :: RiderCtx,
    -- | The ticket or journey the placement is about; Nothing for placements about the rider, and in the manifest.
    subject :: Maybe SubjectCtx,
    history :: HistoryCtx,
    derived :: DerivedCtx,
    answer :: Maybe AnswerCtx
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data RiderCtx = RiderCtx
  { merchantOperatingCityId :: Text,
    cityName :: Text,
    language :: Maybe Text,
    -- | ANDROID / IOS.
    platform :: Maybe Text,
    appVersion :: Maybe Text,
    -- | major * 10000 + minor * 100 + patch (2.5.0 = 20500), for "at least this version" rules.
    appVersionCode :: Maybe Int,
    completedRides :: Int,
    daysSinceSignup :: Int,
    -- | Namma tag names (no values or expiries).
    tags :: [Text]
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data SubjectCtx = SubjectCtx
  { subjectType :: DAFR.AppFeedbackSubjectType,
    ticket :: Maybe TicketCtx,
    journey :: Maybe JourneyCtx
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data TicketCtx = TicketCtx
  { vehicleType :: Text,
    serviceTierType :: Maybe Text,
    status :: Text,
    fare :: Double,
    routeCode :: Maybe Text,
    fromStationCode :: Text,
    toStationCode :: Text,
    isSpotBooking :: Bool,
    providerName :: Text,
    minutesSinceBooked :: Int
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data JourneyCtx = JourneyCtx
  { status :: Text,
    modes :: [Text],
    totalLegs :: Int,
    hasPublicTransport :: Bool,
    estimatedDurationMinutes :: Maybe Int,
    minutesSinceStart :: Maybe Int
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data HistoryCtx = HistoryCtx
  { -- | Feedback cards (app and ride) the rider was shown in the last 7 days.
    cardsShownLast7Days :: Int,
    -- | Days since each app question was last answered, by question key.
    daysSinceAnswered :: Map.Map Text Int
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data DerivedCtx = DerivedCtx
  { -- | Hour of day (0-23) in the city's local time.
    localHour :: Int,
    -- | MON, TUE, ...
    dayOfWeek :: Text,
    -- | Stable per rider, in [0, 99]: the same rider is always in the same bucket.
    rolloutBucket :: Int
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

-- | A loaded subject: what the placement's answers are about.
data Subject = Subject
  { subjectType :: DAFR.AppFeedbackSubjectType,
    subjectId :: Text,
    ctx :: Maybe SubjectCtx
  }

-- | The subject of a placement for this rider. Placements about the rider take no subjectId; the
-- others need one that belongs to the rider.
loadSubject :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => UTCTime -> DP.Person -> DAFR.AppFeedbackSubjectType -> Maybe Text -> m Subject
loadSubject now person subjectType mbSubjectId = case subjectType of
  DAFR.RIDER -> pure Subject {subjectType, subjectId = person.id.getId, ctx = Nothing}
  DAFR.FRFS_BOOKING -> do
    sid <- needId
    booking <- QFTB.findById (Id sid) >>= fromMaybeM (InvalidRequest $ "Ticket booking " <> sid <> " not found")
    unless (booking.riderId == person.id) $ throwError (InvalidRequest $ "Ticket booking " <> sid <> " not found")
    pure Subject {subjectType, subjectId = sid, ctx = Just SubjectCtx {subjectType, ticket = Just (ticketCtx booking), journey = Nothing}}
  DAFR.JOURNEY -> do
    sid <- needId
    journey <- QJourney.findByPrimaryKey (Id sid) >>= fromMaybeM (InvalidRequest $ "Journey " <> sid <> " not found")
    unless (journey.riderId == person.id) $ throwError (InvalidRequest $ "Journey " <> sid <> " not found")
    pure Subject {subjectType, subjectId = sid, ctx = Just SubjectCtx {subjectType, ticket = Nothing, journey = Just (journeyCtx journey)}}
  where
    needId = mbSubjectId & fromMaybeM (InvalidRequest $ "subjectId is needed for " <> show subjectType)
    minutesSince t = floor (Time.diffUTCTime now t / 60)
    ticketCtx (b :: DFTB.FRFSTicketBooking) =
      TicketCtx
        { vehicleType = show b.vehicleType,
          serviceTierType = show <$> b.serviceTierType,
          status = show b.status,
          fare = realToFrac b.totalPrice.amount.getHighPrecMoney,
          routeCode = b.routeCode,
          fromStationCode = b.fromStationCode,
          toStationCode = b.toStationCode,
          isSpotBooking = fromMaybe False b.isSpotBooking,
          providerName = b.providerName,
          minutesSinceBooked = minutesSince b.createdAt
        }
    journeyCtx (j :: DJ.Journey) =
      JourneyCtx
        { status = show j.status,
          modes = map show j.modes,
          totalLegs = j.totalLegs,
          hasPublicTransport = fromMaybe False j.isPublicTransportIncluded,
          estimatedDurationMinutes = (\(Seconds s) -> s `div` 60) <$> j.estimatedDuration,
          minutesSinceStart = minutesSince <$> j.startTime
        }

buildContext ::
  UTCTime ->
  Seconds ->
  DP.Person ->
  DMOC.MerchantOperatingCity ->
  -- | Feedback cards shown in the last 7 days.
  Int ->
  -- | Days since each app question was last answered.
  Map.Map Text Int ->
  Maybe AppFeedbackPlacement ->
  Maybe SubjectCtx ->
  AppFeedbackContext
buildContext now timeDiffFromUtc person city cardsShownLast7Days daysSinceAnswered placement subject =
  AppFeedbackContext
    { placement,
      rider =
        RiderCtx
          { merchantOperatingCityId = city.id.getId,
            cityName = show city.city,
            language = show <$> person.language,
            platform = (\d -> show d.deviceType) <$> person.clientDevice,
            appVersion = versionText <$> person.clientBundleVersion,
            appVersionCode = (\v -> v.major * 10000 + v.minor * 100 + v.maintenance) <$> person.clientBundleVersion,
            completedRides = fromMaybe 0 person.totalRidesCount,
            daysSinceSignup = floor (Time.diffUTCTime now person.createdAt / 86400),
            tags = map (T.takeWhile (/= '#') . (.getTagNameValueExpiry)) (fromMaybe [] person.customerNammaTags)
          },
      subject,
      history = HistoryCtx {cardsShownLast7Days, daysSinceAnswered},
      derived =
        DerivedCtx
          { localHour = Time.todHour (Time.localTimeOfDay localDateTime),
            dayOfWeek = T.toUpper . T.take 3 . show $ Time.dayOfWeek (Time.localDay localDateTime),
            rolloutBucket = stableBucket person.id.getId
          },
      answer = Nothing
    }
  where
    localDateTime = Time.utcToLocalTime Time.utc (Time.addUTCTime (fromIntegral timeDiffFromUtc.getSeconds) now)
    versionText v = T.intercalate "." (map show [v.major, v.minor, v.maintenance])

withAnswer :: LFT.FeedbackAnswer -> AppFeedbackContext -> AppFeedbackContext
withAnswer ans ctx = ctx {answer = Just (mkAnswerCtx ans)}

-- | Sample input the dashboard shows when verifying an APP-FEEDBACK logic (a Bangalore rider on the
-- home screen, on app 2.5.0).
instance Default AppFeedbackContext where
  def =
    AppFeedbackContext
      { placement = Just HOME,
        rider =
          RiderCtx
            { merchantOperatingCityId = "",
              cityName = "Bangalore",
              language = Just "ENGLISH",
              platform = Just "ANDROID",
              appVersion = Just "2.5.0",
              appVersionCode = Just 20500,
              completedRides = 12,
              daysSinceSignup = 90,
              tags = []
            },
        subject = Nothing,
        history = HistoryCtx {cardsShownLast7Days = 0, daysSinceAnswered = Map.empty},
        derived = DerivedCtx {localHour = 18, dayOfWeek = "MON", rolloutBucket = 42},
        answer = Nothing
      }
