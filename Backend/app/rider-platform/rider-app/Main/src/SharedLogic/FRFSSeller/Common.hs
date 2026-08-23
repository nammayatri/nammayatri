module SharedLogic.FRFSSeller.Common
  ( sellerRiderId,
    stationCoords,
    operatorConfig,
    OperatorConfig (..),
    OperatorCancellation (..),
    OperatorRecon (..),
    OperatorCatalog (..),
    OperatorQuoteCache (..),
    isSellerRider,
    operatorMerchantShortId,
    metroProviderId,
    SellerJourneyType (..),
    MetroJourneyCode (..),
    metroJourneyCode,
    sellerJourneyTypes,
    journeyTypeForItemId,
    maxTicketsPerOrder,
    TicketValidity (..),
    ticketValidity,
    journeyIdFromStationNames,
    formatPrice,
    nonZeroBuyerFinderFee,
    settlementAccount,
    sellerSearchId,
    sellerIssueId,
    sellerReconId,
    SellerErrorCode (..),
    becknError,
    errorCodeText,
    operatorErrorCode,
    Serviceability (..),
    serviceabilityOf,
    sellerOrderId,
    orderIdPrefixesFor,
    prefixFor,
  )
where

import qualified Beckn.ACL.FRFSSeller.OnInit as OnInitACL
import qualified BecknV2.FRFS.Types as Spec
import qualified Data.ByteString as BS
import Data.Char (isAlphaNum, isAscii)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.UUID as UU
import qualified Data.UUID.V5 as UUV5
import qualified Domain.Types as BknTypes
import qualified Domain.Types.BecknConfig as DBC
import Domain.Types.Extra.IntegratedBPPConfig (OperatorCancellation (..), OperatorCatalog (..), OperatorConfig (..), OperatorQuoteCache (..), OperatorRecon (..))
import qualified Domain.Types.FRFSQuote as DQuote
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Person as DP
import qualified ExternalBPP.ExternalAPI.Metro.CMRL.StationList as CMRLStationList
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Text (decodeFromText)
import Numeric (showFFloat)

sellerRiderId :: Id DP.Person
sellerRiderId = Id "frfsSeller_rider_id"

isSellerRider :: Id DP.Person -> Bool
isSellerRider = (== sellerRiderId)

operatorMerchantShortId :: Text -> ShortId a
operatorMerchantShortId slug = ShortId $ "FRFS_SELLER_" <> T.toUpper slug

metroProviderId :: Text -> Text
metroProviderId operator = "triffy-" <> T.toLower operator <> "-rail-metro"

sellerJourneyTypes :: [SellerJourneyType]
sellerJourneyTypes =
  [ SellerJourneyType {quoteType = DQuote.SingleJourney, code = SJT, name = "Single Journey Ticket"},
    SellerJourneyType {quoteType = DQuote.ReturnJourney, code = RJT, name = "Return Journey Ticket"}
  ]

data SellerJourneyType = SellerJourneyType
  { quoteType :: DQuote.FRFSQuoteType,
    code :: MetroJourneyCode,
    name :: Text
  }
  deriving (Show, Eq)

-- | Journey code the metro operator APIs (CDAC/CMRL, KMRL) and ONDC item ids speak.
-- 'show' is the wire form, so the constructor names are the codes.
data MetroJourneyCode = SJT | RJT
  deriving (Show, Read, Eq)

metroJourneyCode :: DQuote.FRFSQuoteType -> MetroJourneyCode
metroJourneyCode = \case
  DQuote.ReturnJourney -> RJT
  _ -> SJT

journeyTypeForItemId :: Text -> Maybe SellerJourneyType
journeyTypeForItemId itemId = do
  prefix <- listToMaybe (T.splitOn "-" itemId)
  code <- readMaybe (T.unpack (T.toUpper prefix))
  find (\journeyType -> journeyType.code == code) sellerJourneyTypes

maxTicketsPerOrder :: OperatorConfig -> MetroJourneyCode -> Int
maxTicketsPerOrder config = \case
  RJT -> config.roundTripTicketLimit
  SJT -> config.oneWayTicketLimit

ticketValidity :: OperatorConfig -> TicketValidity
ticketValidity config =
  TicketValidity {label = config.ticketValidityLabel, duration = config.ticketValidityDuration}

data TicketValidity = TicketValidity
  { label :: Text,
    duration :: Text
  }
  deriving (Show, Eq)

stationCoords :: [CMRLStationList.Station] -> Text -> (Maybe Double, Maybe Double)
stationCoords roster code =
  case find matches roster of
    Nothing -> (Nothing, Nothing)
    Just station -> (nonZero station.latitude, nonZero station.longitude)
  where
    matches station = station.stationId == code || station.code <> "|" <> station.stationId == code
    nonZero v = if v == 0.0 then Nothing else Just v

operatorConfig :: Maybe OperatorConfig -> Either Text OperatorConfig
operatorConfig = maybe (Left "integrated_bpp_config.operator_config is not set for this operator") Right

journeyIdFromStationNames :: Text -> Text -> Maybe Text
journeyIdFromStationNames fromName toName = do
  from <- nonEmpty' (stripToAlphaNum fromName)
  to <- nonEmpty' (stripToAlphaNum toName)
  pure $ from <> "-" <> to
  where
    stripToAlphaNum = T.filter (\c -> isAscii c && isAlphaNum c) . T.toLower
    nonEmpty' t = if T.null t then Nothing else Just t

formatPrice :: Double -> Text
formatPrice value = T.pack (showFFloat (Just 2) value "")

nonZeroBuyerFinderFee :: Spec.Order -> Maybe Text
nonZeroBuyerFinderFee order = do
  payments <- order.orderPayments
  let tagValues =
        [ (tag.tagDescriptor >>= (.descriptorCode), tag.tagValue)
          | payment <- payments,
            tagGroup <- fromMaybe [] payment.paymentTags,
            tag <- fromMaybe [] tagGroup.tagGroupList
        ]
  find (not . isZeroFee) $
    [value | (Just code, Just value) <- tagValues, T.toUpper code == "BUYER_FINDER_FEES_PERCENTAGE"]
  where
    isZeroFee value = readMaybe (T.unpack value) == Just (0 :: Double)

settlementAccount :: DBC.BecknConfig -> Text -> Either Text OnInitACL.SettlementAccount
settlementAccount becknConfig settlementAmount = do
  params <-
    (decodeFromText =<< becknConfig.paymentParamsJson :: Maybe BknTypes.BknPaymentParams)
      & maybe (Left "beckn_config.payment_params_json missing or unparseable") Right
  bankAccountNumber <- params.bankAccNumber & maybe (Left "Settlement bank account number not configured") Right
  bankCode <- params.bankCode & maybe (Left "Settlement bank code not configured") Right
  pure
    OnInitACL.SettlementAccount
      { bankAccountNumber,
        bankCode,
        settlementAmount,
        settlementType = becknConfig.settlementType
      }

sellerSearchId :: Text -> Text -> Text
sellerSearchId operator transactionId =
  UU.toText . UUV5.generateNamed UU.nil . BS.unpack . TE.encodeUtf8 $
    "frfsSeller:" <> operator <> ":" <> transactionId

sellerIssueId :: Text -> Text
sellerIssueId issueId =
  UU.toText . UUV5.generateNamed UU.nil . BS.unpack . TE.encodeUtf8 $
    "frfsSeller:igm:" <> issueId

sellerReconId :: Text -> Text
sellerReconId orderId =
  UU.toText . UUV5.generateNamed UU.nil . BS.unpack . TE.encodeUtf8 $
    "frfsSeller:recon:" <> orderId

data Serviceability = Serviceable | Unserviceable Text
  deriving (Eq, Show)

-- | Unset allows every buyer; set-but-silent-on-this-city refuses.
serviceabilityOf :: Maybe (HM.HashMap Text [Text]) -> Text -> Maybe Text -> Serviceability
serviceabilityOf Nothing _ _ = Serviceable
serviceabilityOf (Just byCity) cityCode mbBapId =
  case HM.lookup cityCode byCity of
    Nothing -> Unserviceable ("city " <> cityCode <> " is not serviced")
    Just allowed -> case mbBapId of
      Nothing -> Unserviceable "no bap id on the context"
      Just bapId
        | bapId `elem` allowed -> Serviceable
        | otherwise -> Unserviceable ("buyer " <> bapId <> " is not permitted in " <> cityCode)

-- | The ONDC @order.id@: a fresh uuid, dashes stripped, behind a per-buyer prefix. Buyers
-- and settlement both read this id, so its shape is a contract.
sellerOrderId :: Maybe (HM.HashMap Text Text) -> Text -> Text -> Text -> Text
sellerOrderId prefixes fallback bapId rawUuid =
  prefixFor prefixes bapId fallback <> T.filter (\c -> isAscii c && isAlphaNum c) rawUuid

orderIdPrefixesFor :: DIBC.ProviderConfig -> Maybe (HM.HashMap Text Text)
orderIdPrefixesFor = \case
  DIBC.KMRL config -> config.bapOrderIdPrefixes
  _ -> Nothing

-- | Per-buyer prefix, keyed by the exact bap_id. A buyer the map does not name, or names
-- with an empty string, falls back.
prefixFor :: Maybe (HM.HashMap Text Text) -> Text -> Text -> Text
prefixFor mbPrefixes bapId fallback =
  case HM.lookup (T.toLower bapId) =<< mbPrefixes of
    Just prefix | not (T.null prefix) -> prefix
    _ -> fallback

-- | The BPP error contract, transcribed from @common_constants.go:112-126@ and the
-- published reference at @docs/bap-error-code-reference.md@. Codes are the buyer's
-- contract -- they switch on them -- so they live in ONE enumeration here rather than as
-- literals at each throw site, which is how three invented codes (30004, 30005, 40002)
-- reached this migration in the first place.
data SellerErrorCode
  = -- | @TF_METRO_INTERNAL_ERROR@ is the operator-AFCS upstream case; this is our own
    -- last-resort fallback. FATAL: buyers escalate on it, so prefer a specific code.
    InternalError
  | -- | @TF_METRO_UPSTREAM_UNAVAILABLE@ (31003). TRANSIENT -- tells the buyer no ticket
    -- was issued and the confirm is safe to retry.
    OperatorUnavailable
  | -- | @TF_METRO_ORDER_NOT_FOUND@ (31002).
    OrderNotFound
  | -- | @TF_METRO_ITEM_NOT_FOUND@ (91215) -- stale or unresolvable item id.
    ItemNotFound
  | -- | @TF_METRO_ITEM_QTY_EXCEEDS@ (91204).
    ItemQuantityExceeded
  | -- | @TF_METRO_FINDER_FEE_NOT_ACCEPTABLE@ (41001).
    FinderFeeNotAcceptable
  | -- | @TF_METRO_CANCELLATION_NOT_POSSIBLE@ (50001).
    CancellationNotPossible
  | -- | @TF_METRO_LOCATION_NOT_SERVICEABLE@ (30008).
    LocationUnserviceable
  | -- | @TF_METRO_WRONG_FARE@ (91214).
    WrongFare
  | -- | @TF_METRO_STATION_NON_SERVICEABLE@ (91201).
    StationNotServiceable
  | -- | @TF_DUPLICATE_REQUEST@ (91213).
    DuplicateRequest
  | -- | @TF_METRO_CANNOT_CANCEL_USED_TICKET@ (91216).
    CannotCancelUsedTicket
  | -- | @TF_METRO_INTERNAL_ERROR@ (30001) -- the operator said something we do not
    -- recognise. TRANSIENT, and deliberately distinct from our own @InternalError@.
    OperatorRejected
  deriving (Eq, Show)

errorCodeText :: SellerErrorCode -> Text
errorCodeText = \case
  InternalError -> "31001"
  OperatorUnavailable -> "31003"
  OrderNotFound -> "31002"
  ItemNotFound -> "91215"
  ItemQuantityExceeded -> "91204"
  FinderFeeNotAcceptable -> "41001"
  CancellationNotPossible -> "50001"
  LocationUnserviceable -> "30008"
  WrongFare -> "91214"
  StationNotServiceable -> "91201"
  DuplicateRequest -> "91213"
  CannotCancelUsedTicket -> "91216"
  OperatorRejected -> "30001"

-- | The short, stable human label. The contract promises this does not change for a
-- given code, so it is fixed here and never built from the failure's own text.
errorCodeMessage :: SellerErrorCode -> Text
errorCodeMessage = \case
  InternalError -> "Internal Error"
  OperatorUnavailable -> "Operator temporarily unavailable"
  OrderNotFound -> "Order not found"
  ItemNotFound -> "Item not found"
  ItemQuantityExceeded -> "Item quantity exceeded"
  FinderFeeNotAcceptable -> "Finder fee not acceptable"
  CancellationNotPossible -> "Cancellation not possible"
  LocationUnserviceable -> "Location unserviceable"
  WrongFare -> "Wrong fare while booking ticket"
  StationNotServiceable -> "Route Serviceability error"
  DuplicateRequest -> "Stale Request"
  CannotCancelUsedTicket -> "Cannot cancel used ticket"
  OperatorRejected -> "Internal Error Occured"

-- | Build the wire error: stable @code@ and @message@ from the table, and the contextual
-- @description@ the contract reserves for display to the user.
becknError :: SellerErrorCode -> Text -> Spec.Error
becknError code description =
  Spec.Error
    { Spec.errorCode = Just (errorCodeText code),
      Spec.errorMessage = Just (errorCodeMessage code),
      Spec.errorPaths = Nothing,
      Spec.errorDescription = Just description
    }

-- | The operators do not speak ONDC codes -- they return free-text English, which the Go
-- service translates through a per-operator lookup: @kochi_metro.go:214@ (seven strings)
-- and @chennai_metro.go:53@ (one). Both maps are transcribed here as a union, which is
-- safe because no string appears in both and neither operator emits the other\'s wording.
--
-- TWO THINGS THIS DOES NOT YET REPRODUCE, both needing the typed operator error rather
-- than the rendered exception text we currently have at the call site:
--   * Go matches the AFCS message EXACTLY; we test for containment, because the message
--     reaches us wrapped inside a rendered exception.
--   * Go answers @30001@ when an AFCS message arrived but matched nothing, reserving
--     @31003@ for failures with no operator message at all. We cannot yet tell those
--     apart, so an unmatched failure falls back to @31003@ at the call site.
operatorErrorCode :: Text -> Maybe SellerErrorCode
operatorErrorCode message =
  fmap snd . find (\(needle, _) -> needle `T.isInfixOf` message) $
    [ ("Fare is not valid", WrongFare),
      ("The Ticket fare value was wrong", WrongFare),
      ("Invalid source or destination station id", StationNotServiceable),
      ("Please share a unique transactionId for booking a new ticket", DuplicateRequest),
      ("This ticket cannot be cancelled as your QR code is already validated at AFC metro gates.", CannotCancelUsedTicket),
      ("You cannot cancel used ticket", CannotCancelUsedTicket),
      ("Book ticket limit exceeded", ItemQuantityExceeded),
      ("Ticket is not refundable!", CancellationNotPossible)
    ]
