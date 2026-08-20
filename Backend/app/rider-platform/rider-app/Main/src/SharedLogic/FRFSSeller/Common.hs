module SharedLogic.FRFSSeller.Common
  ( sellerRiderId,
    stationCoords,
    catalogBrandName,
    catalogBrandLogoUrl,
    operatorTerms,
    OperatorTerms (..),
    isSellerRider,
    operatorMerchantShortId,
    metroProviderId,
    SellerJourneyType (..),
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
  )
where

import qualified Beckn.ACL.FRFSSeller.OnInit as OnInitACL
import qualified BecknV2.FRFS.Types as Spec
import qualified Data.ByteString as BS
import Data.Char (isAlphaNum, isAscii)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.UUID as UU
import qualified Data.UUID.V5 as UUV5
import qualified Domain.Types as BknTypes
import qualified Domain.Types.BecknConfig as DBC
import qualified Domain.Types.FRFSQuote as DQuote
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
  [ SellerJourneyType {quoteType = DQuote.SingleJourney, code = "SJT", name = "Single Journey Ticket", ticketTypeId = Nothing},
    SellerJourneyType {quoteType = DQuote.ReturnJourney, code = "RJT", name = "Return Journey Ticket", ticketTypeId = Just 102}
  ]

data SellerJourneyType = SellerJourneyType
  { quoteType :: DQuote.FRFSQuoteType,
    code :: Text,
    name :: Text,
    ticketTypeId :: Maybe Int
  }
  deriving (Show, Eq)

journeyTypeForItemId :: Text -> Maybe SellerJourneyType
journeyTypeForItemId itemId = do
  prefix <- listToMaybe (T.splitOn "-" itemId)
  find (\journeyType -> T.toLower journeyType.code == T.toLower prefix) sellerJourneyTypes

maxTicketsPerOrder :: Text -> Text -> Int
maxTicketsPerOrder operator journeyTypeCode =
  case (T.toLower operator, T.toUpper journeyTypeCode) of
    ("kmrl", "RJT") -> 1
    _ -> 6

ticketValidity :: Text -> TicketValidity
ticketValidity operator =
  case T.toLower operator of
    "kmrl" -> TicketValidity {label = "Validity", duration = "PT120M"}
    _ -> TicketValidity {label = "Validity", duration = "PT1D"}

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

catalogBrandName :: Text
catalogBrandName = "Triffy Metro"

catalogBrandLogoUrl :: Text
catalogBrandLogoUrl = "https://framerusercontent.com/images/FnrHHdp6nMaW64THdqvZnnTAc.png"

operatorTerms :: Text -> OperatorTerms
operatorTerms operator =
  case T.toLower operator of
    "kmrl" ->
      OperatorTerms
        { businessTermsUrl = "https://metro-terms.movingtech.in/kochi/index.html",
          cancellationTermsUrl = "https://kochimetro.org/contactless-ticketing-digital-payment-guidelines/",
          courtJurisdiction = "Kochi",
          maxPaidAreaMinutes = Nothing
        }
    _ ->
      OperatorTerms
        { businessTermsUrl = "https://metro-terms.movingtech.in/chennai/index.html",
          cancellationTermsUrl = "https://metro-terms.movingtech.in/chennai/index.html",
          courtJurisdiction = "Chennai",
          maxPaidAreaMinutes = Just 170
        }

data OperatorTerms = OperatorTerms
  { businessTermsUrl :: Text,
    cancellationTermsUrl :: Text,
    courtJurisdiction :: Text,
    maxPaidAreaMinutes :: Maybe Int
  }
  deriving (Show, Eq)

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
