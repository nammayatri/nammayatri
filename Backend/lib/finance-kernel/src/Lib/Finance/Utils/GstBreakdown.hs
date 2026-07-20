{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Lib.Finance.Utils.GstBreakdown
  ( GstJurisdiction (..),
    GstRateBreakup (..),
    GstRateInfraStateBreakup (..),
    GstRateInterStateBreakup (..),
    compareIndianPlace,
    compareIndianGstinStateCode,
    computeGstBreakdownFromRates,
    defaultIntraStateGstBreakdown,
  )
where

import qualified Data.Text as T
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)
import Lib.Finance.Invoice.Interface (GstAmountBreakdown (..))

data GstJurisdiction = IntraState | InterState
  deriving (Eq, Show)

-- The sum of types was made to avoid mutually exclusive tax rates
data GstRateBreakup = InfraStateBreakup GstRateInfraStateBreakup | InterStateBreakup GstRateInterStateBreakup
  deriving (Eq, Show)

data GstRateInfraStateBreakup = GstRateInfraStateBreakup
  { cgstRate :: Maybe HighPrecMoney,
    sgstRate :: Maybe HighPrecMoney
  }
  deriving (Eq, Show)

newtype GstRateInterStateBreakup = GstRateInterStateBreakup
  { igstRate :: Maybe HighPrecMoney
  }
  deriving (Eq, Show)

normalizeGeoComponent :: Maybe Text -> Maybe Text
normalizeGeoComponent mbText =
  case T.toLower . T.strip <$> mbText of
    Just value | not (T.null value) -> Just value
    _ -> Nothing

-- | Compare two Indian state names (case-insensitive).
compareIndianState :: Maybe Text -> Maybe Text -> Maybe GstJurisdiction
compareIndianState supplierState receiverState =
  case (normalizeGeoComponent supplierState, normalizeGeoComponent receiverState) of
    (Just leftState, Just rightState) ->
      Just $
        if leftState == rightState
          then IntraState
          else InterState
    _ -> Nothing

-- | Compare states first; fall back to city names when either state is missing.
compareIndianPlace ::
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe GstJurisdiction
compareIndianPlace supplierState receiverState supplierCity receiverCity =
  case compareIndianState supplierState receiverState of
    Just jurisdiction -> Just jurisdiction
    Nothing ->
      case (normalizeGeoComponent supplierCity, normalizeGeoComponent receiverCity) of
        (Just leftCity, Just rightCity) ->
          Just $
            if leftCity == rightCity
              then IntraState
              else InterState
        _ -> Nothing

normaliseGstin :: Maybe Text -> Maybe Text
normaliseGstin mbGstin = do
  gstin <- T.toUpper . T.strip <$> mbGstin
  guard (T.length gstin >= 2)
  pure gstin

-- | Compare the first two characters (state code) of seller and buyer GSTINs.
compareIndianGstinStateCode :: Maybe Text -> Maybe Text -> Maybe GstJurisdiction
compareIndianGstinStateCode sellerGstin buyerGstin =
  case (normaliseGstin sellerGstin, normaliseGstin buyerGstin) of
    (Just seller, Just buyer) ->
      Just $
        if T.take 2 seller == T.take 2 buyer
          then IntraState
          else InterState
    _ -> Nothing

-- | Split a total GST amount into CGST/SGST or IGST proportionally based on GstRateBreakup percentages.
--   Non-positive rates are ignored (neither in the split nor in totalPct), so remaining components sum to totalGst.
--   If the total percentage is 0, returns Nothing.
computeGstBreakdownFromRates :: GstRateBreakup -> HighPrecMoney -> Maybe GstAmountBreakdown
computeGstBreakdownFromRates rateBreakup totalGst
  | totalGst <= 0 = Nothing
  | otherwise = case rateBreakup of
    InfraStateBreakup GstRateInfraStateBreakup {cgstRate, sgstRate} ->
      let cgstPct = positiveRate cgstRate
          sgstPct = positiveRate sgstRate
          totalPct = cgstPct + sgstPct
       in if totalPct <= 0
            then Nothing
            else
              Just
                GstAmountBreakdown
                  { cgstAmount = if cgstPct > 0 then Just (totalGst * cgstPct / totalPct) else Nothing,
                    sgstAmount = if sgstPct > 0 then Just (totalGst * sgstPct / totalPct) else Nothing,
                    igstAmount = Nothing
                  }
    InterStateBreakup GstRateInterStateBreakup {igstRate} ->
      if positiveRate igstRate <= 0
        then Nothing
        else
          Just
            GstAmountBreakdown
              { cgstAmount = Nothing,
                sgstAmount = Nothing,
                igstAmount = Just totalGst
              }
  where
    positiveRate = maybe 0 (\pct -> if pct > 0 then pct else 0)

-- | Conservative fallback when jurisdiction is unknown: assume intra-state 50/50 CGST/SGST.
defaultIntraStateGstBreakdown :: HighPrecMoney -> GstAmountBreakdown
defaultIntraStateGstBreakdown taxAmount =
  let half = taxAmount / 2.0
   in GstAmountBreakdown {cgstAmount = Just half, sgstAmount = Just (taxAmount - half), igstAmount = Nothing}
