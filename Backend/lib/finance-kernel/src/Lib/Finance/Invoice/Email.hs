module Lib.Finance.Invoice.Email
  ( InvoiceEmailConfig (..),
    InvoiceEmailData (..),
    InvoiceEmailPayment (..),
    InvoiceEmailStop (..),
    InvoiceEmailLocale (..),
    InvoiceEmailTemplateValue (..),
    RenderedInvoiceEmail (..),
    renderInvoiceEmail,
    mkInvoiceEmailLocale,
    formatEmailDate,
    formatEmailClock,
    formatEmailStampDate,
    formatEmailDuration,
    formatEmailDistance,
    formatEmailMoney,
    currencySymbol,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Time as DT
import Kernel.Prelude
import Kernel.Types.Common (Currency (..), HighPrecMeters (..), HighPrecMoney (..))
import Lib.Finance.Invoice.RenderTemplate (buildTemplateChecked, escHtml)
import Text.Printf (printf)

data InvoiceEmailConfig = InvoiceEmailConfig
  { enabled :: Bool,
    sendOnRideEnd :: Bool,
    fromEmail :: Text,
    appName :: Text,
    attachmentNamePrefix :: Text,
    logoUrl :: Text,
    routeIconUrl :: Text,
    cashIconUrl :: Text,
    cardIconUrls :: Map.Map Text Text,
    defaultCardIconUrl :: Maybe Text,
    use24HourClock :: Bool,
    sendDelaySeconds :: Int,
    retryIntervalSeconds :: Int,
    maxRetries :: Int
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

data InvoiceEmailPayment = InvoiceEmailPayment
  { iconUrl :: Maybe Text,
    isCash :: Bool,
    cardLabel :: Text,
    stamp :: Maybe Text
  }
  deriving (Generic, Show, Eq)

data InvoiceEmailStop = InvoiceEmailStop
  { timeLabel :: Text,
    address :: Text
  }
  deriving (Generic, Show, Eq)

data InvoiceEmailData = InvoiceEmailData
  { appName :: Text,
    logoUrl :: Text,
    vehicleIconUrl :: Maybe Text,
    bookingDate :: Text,
    bookingTime :: Text,
    serviceTierLabel :: Text,
    price :: Text,
    payment :: Maybe InvoiceEmailPayment,
    vehicleModel :: Maybe Text,
    driverName :: Maybe Text,
    rideDistance :: Maybe Text,
    estimatedDistance :: Maybe Text,
    rideDuration :: Maybe Text,
    estimatedDuration :: Maybe Text,
    rideShortId :: Text,
    pickup :: InvoiceEmailStop,
    dropoff :: Maybe InvoiceEmailStop,
    routeIconUrl :: Text,
    invoiceNumber :: Text
  }
  deriving (Generic, Show, Eq)

data InvoiceEmailLocale = InvoiceEmailLocale
  { weekdayNames :: [Text],
    monthNames :: [Text]
  }
  deriving (Generic, Show, Eq)

data InvoiceEmailTemplateValue = InvoiceEmailTemplateValue
  { subjectTemplate :: Text,
    bodyTemplate :: Text,
    monthNames :: Maybe [Text],
    weekdayNames :: Maybe [Text]
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

data RenderedInvoiceEmail = RenderedInvoiceEmail
  { subject :: Text,
    html :: Text,
    missingKeys :: [Text]
  }
  deriving (Generic, Show, Eq)

mkInvoiceEmailLocale :: InvoiceEmailTemplateValue -> InvoiceEmailLocale
mkInvoiceEmailLocale templateValue =
  InvoiceEmailLocale
    { weekdayNames = withDefault englishWeekdays 7 templateValue.weekdayNames,
      monthNames = withDefault englishMonths 12 templateValue.monthNames
    }
  where
    withDefault def n = maybe def (\xs -> if length xs == n then xs else def)
    englishWeekdays = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]
    englishMonths = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec"]

formatEmailDate :: InvoiceEmailLocale -> DT.LocalTime -> Text
formatEmailDate locale localTime =
  let day = DT.localDay localTime
      (_, month, dayOfMonth) = DT.toGregorian day
      weekdayIdx = fromEnum (DT.dayOfWeek day) - 1
      weekday = fromMaybe "" (atIndex weekdayIdx locale.weekdayNames)
      monthName = fromMaybe "" (atIndex (month - 1) locale.monthNames)
   in weekday <> ", " <> show dayOfMonth <> " " <> monthName

formatEmailClock :: Bool -> DT.LocalTime -> Text
formatEmailClock use24Hour localTime =
  let tod = DT.localTimeOfDay localTime
      hour = DT.todHour tod
      minute = DT.todMin tod
      twoDigits n = T.justifyRight 2 '0' (show n)
   in if use24Hour
        then twoDigits hour <> ":" <> twoDigits minute
        else
          let hour12 = if hour `mod` 12 == 0 then 12 else hour `mod` 12
              suffix = if hour < 12 then "am" else "pm"
           in show hour12 <> ":" <> twoDigits minute <> suffix

formatEmailStampDate :: DT.LocalTime -> Text
formatEmailStampDate localTime = T.pack (DT.formatTime DT.defaultTimeLocale "%d.%m.%Y" localTime)

formatEmailDuration :: Int -> Text
formatEmailDuration totalSeconds =
  let secs = max 0 totalSeconds
      hours = secs `div` 3600
      minutes = (secs `mod` 3600) `div` 60
      seconds = secs `mod` 60
      parts =
        catMaybes
          [ if hours > 0 then Just (show hours <> " h") else Nothing,
            if minutes > 0 then Just (show minutes <> " min") else Nothing,
            if seconds > 0 || (hours == 0 && minutes == 0) then Just (show seconds <> " s") else Nothing
          ]
   in T.unwords parts

formatEmailDistance :: HighPrecMeters -> Text
formatEmailDistance meters =
  let km = realToFrac (getHighPrecMeters meters) / 1000 :: Double
      rounded = fromIntegral (round (km * 10) :: Integer) / 10 :: Double
      whole = round rounded :: Integer
   in if rounded == fromIntegral whole
        then show whole <> " km"
        else T.pack (printf "%.1f" rounded) <> " km"

formatEmailMoney :: Currency -> HighPrecMoney -> Text
formatEmailMoney currency amount =
  currencySymbol currency <> " " <> T.pack (printf "%.2f" (realToFrac (getHighPrecMoney amount) :: Double))

currencySymbol :: Currency -> Text
currencySymbol = \case
  EUR -> "€"
  INR -> "₹"
  USD -> "$"

renderInvoiceEmail :: Text -> Text -> InvoiceEmailData -> RenderedInvoiceEmail
renderInvoiceEmail subjectTemplate htmlTemplate emailData =
  let (renderedSubject, subjectMissing) = buildTemplateChecked (subjectVars emailData) subjectTemplate
      (renderedHtml, htmlMissing) = buildTemplateChecked (map (\(k, v) -> (k, escHtml v)) (htmlVars emailData)) htmlTemplate
   in RenderedInvoiceEmail
        { subject = T.strip renderedSubject,
          html = renderedHtml,
          missingKeys = subjectMissing <> htmlMissing
        }

subjectVars :: InvoiceEmailData -> [(Text, Text)]
subjectVars d =
  [ ("appName", d.appName),
    ("bookingDate", d.bookingDate),
    ("invoiceNumber", d.invoiceNumber),
    ("rideShortId", d.rideShortId)
  ]

htmlVars :: InvoiceEmailData -> [(Text, Text)]
htmlVars d =
  [ ("appName", d.appName),
    ("logoUrl", d.logoUrl),
    ("vehicleIconUrl", fromMaybe "" d.vehicleIconUrl),
    ("vehicleIconStyle", hiddenUnless (isJust d.vehicleIconUrl)),
    ("bookingDate", d.bookingDate),
    ("bookingTime", d.bookingTime),
    ("serviceTier", d.serviceTierLabel),
    ("price", d.price),
    ("paymentSectionStyle", hiddenUnless (isJust d.payment)),
    ("paymentIconUrl", fromMaybe "" (d.payment >>= (.iconUrl))),
    ("paymentIconStyle", hiddenUnless (isJust (d.payment >>= (.iconUrl)))),
    ("cashLabelStyle", hiddenUnless (maybe False (.isCash) d.payment)),
    ("cardLabelStyle", hiddenUnless (maybe False (not . (.isCash)) d.payment)),
    ("cardLabel", maybe "" (.cardLabel) d.payment),
    ("paymentStamp", fromMaybe "" (d.payment >>= (.stamp))),
    ("paymentStampStyle", hiddenUnless (isJust (d.payment >>= (.stamp)))),
    ("vehicleModel", fromMaybe "" d.vehicleModel),
    ("vehicleModelStyle", hiddenUnless (isJust d.vehicleModel)),
    ("driverName", fromMaybe "" d.driverName),
    ("driverStyle", hiddenUnless (isJust d.driverName)),
    ("rideDistance", fromMaybe "" d.rideDistance),
    ("rideDistanceStyle", hiddenUnless (isJust d.rideDistance || isJust d.estimatedDistance)),
    ("estimatedDistance", fromMaybe "" d.estimatedDistance),
    ("estimatedDistanceStyle", hiddenUnless (isJust d.estimatedDistance)),
    ("rideDuration", fromMaybe "" d.rideDuration),
    ("rideDurationStyle", hiddenUnless (isJust d.rideDuration || isJust d.estimatedDuration)),
    ("estimatedDuration", fromMaybe "" d.estimatedDuration),
    ("estimatedDurationStyle", hiddenUnless (isJust d.estimatedDuration)),
    ("rideShortId", d.rideShortId),
    ("pickupTime", d.pickup.timeLabel),
    ("pickupAddress", d.pickup.address),
    ("dropStyle", hiddenUnless (isJust d.dropoff)),
    ("dropTime", maybe "" (.timeLabel) d.dropoff),
    ("dropAddress", maybe "" (.address) d.dropoff),
    ("routeIconUrl", d.routeIconUrl),
    ("routeIconStyle", hiddenUnless (isJust d.dropoff)),
    ("invoiceNumber", d.invoiceNumber)
  ]

hiddenUnless :: Bool -> Text
hiddenUnless visible = if visible then "" else "display:none;"

atIndex :: Int -> [a] -> Maybe a
atIndex i xs
  | i < 0 = Nothing
  | otherwise = listToMaybe (drop i xs)
