{-# OPTIONS_GHC -Wwarn=unused-imports #-}

{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.DriverFinancialStatement
  ( listStatements,
    generateStatement,
    downloadStatement,
    getStatementSummary,
    GenerateStatementReq (..),
    StatementListRes (..),
    StatementSummaryRes (..),
    StatementDownloadRes (..),
  )
where

import qualified Crypto.Hash as Hash
import qualified Data.ByteArray as BA
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (utctDay)
import qualified Domain.Types.DriverFee as DDF
import qualified Domain.Types.DriverFinancialStatement as DDFS
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Environment (Flow)
import EulerHS.Prelude hiding (id)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM, generateGUID, getCurrentTime, logInfo)
import qualified Storage.Queries.DriverFeeExtra as QDriverFee
import qualified Storage.Queries.DriverFinancialStatement as QDFS
import qualified Storage.Queries.DriverFinancialStatementExtra as QDFSE
import qualified Storage.Queries.Person as QPerson

-- | Request to generate a financial statement for a custom date range.
data GenerateStatementReq = GenerateStatementReq
  { periodStart :: UTCTime,
    periodEnd :: UTCTime,
    periodType :: DDFS.StatementPeriodType
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

-- | Response listing available financial statements.
data StatementListRes = StatementListRes
  { statements :: [DDFS.DriverFinancialStatement],
    totalCount :: Int
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

-- | Summary view of a statement (without PDF download).
data StatementSummaryRes = StatementSummaryRes
  { statementId :: Id DDFS.DriverFinancialStatement,
    referenceNumber :: Text,
    periodType :: DDFS.StatementPeriodType,
    periodStart :: UTCTime,
    periodEnd :: UTCTime,
    grossEarnings :: HighPrecMoney,
    platformFees :: HighPrecMoney,
    subscriptionCharges :: HighPrecMoney,
    incentives :: HighPrecMoney,
    penalties :: HighPrecMoney,
    tollReimbursements :: HighPrecMoney,
    gstCollected :: HighPrecMoney,
    tdsDeducted :: HighPrecMoney,
    netEarnings :: HighPrecMoney,
    totalPayouts :: HighPrecMoney,
    openingBalance :: HighPrecMoney,
    closingBalance :: HighPrecMoney,
    currency :: Currency,
    status :: DDFS.StatementStatus
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

-- | Response containing the PDF download URL.
data StatementDownloadRes = StatementDownloadRes
  { statementId :: Id DDFS.DriverFinancialStatement,
    pdfUrl :: Maybe Text,
    status :: DDFS.StatementStatus
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

-- | List all financial statements for the authenticated driver.
-- Uses DB-level pagination via LIMIT/OFFSET.
listStatements ::
  ( Maybe (Id DP.Person),
    Id DM.Merchant,
    Id DMOC.MerchantOperatingCity
  ) ->
  Maybe Int ->
  Maybe Int ->
  Flow StatementListRes
listStatements (mbDriverId, _merchantId, _merchantOpCityId) mbLimit mbOffset = do
  driverId <- mbDriverId & fromMaybeM (PersonNotFound "No person found")
  _driver <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  let limit = min 50 . fromMaybe 10 $ mbLimit
      offset = fromMaybe 0 mbOffset
  -- M1 fix: Use DB-level LIMIT/OFFSET instead of in-memory pagination
  paged <- QDFSE.findAllByDriverIdWithLimitAndOffset driverId limit offset
  allStatements <- QDFS.findAllByDriverId driverId
  let total = length allStatements
  pure $
    StatementListRes
      { statements = paged,
        totalCount = total
      }

-- | Generate a new financial statement for a custom date range.
-- Aggregates actual ride earnings, platform fees, subscription charges,
-- incentives, penalties, and computes net earnings and verification hash.
generateStatement ::
  ( Maybe (Id DP.Person),
    Id DM.Merchant,
    Id DMOC.MerchantOperatingCity
  ) ->
  GenerateStatementReq ->
  Flow DDFS.DriverFinancialStatement
generateStatement (mbDriverId, merchantId, merchantOpCityId) req = do
  driverId <- mbDriverId & fromMaybeM (PersonNotFound "No person found")
  _driver <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)

  -- M2 fix: Validate that periodEnd > periodStart
  when (req.periodEnd <= req.periodStart) $
    throwError (InvalidRequest "periodEnd must be after periodStart")

  -- M3 fix: Check for duplicate/overlapping statement for the same period
  existingStatement <- QDFS.findByDriverIdAndPeriod driverId req.periodStart req.periodEnd
  when (isJust existingStatement) $
    throwError (InvalidRequest "A statement already exists for this period")

  now <- getCurrentTime
  statementId <- generateGUID
  refNum <- generateReferenceNumber driverId now

  logInfo $ "Generating financial statement for driver " <> driverId.getId <> " from " <> show req.periodStart <> " to " <> show req.periodEnd

  -- C1 fix: Aggregate actual financial data from driver fees in the period
  driverFees <- QDriverFee.findWindowsWithoutLimit driverId req.periodStart req.periodEnd

  let grossEarnings = sum $ map (\df -> df.totalEarnings) driverFees
      platformFeeAmount = sum $ map (\df -> df.platformFee.fee) driverFees
      cgstAmount = sum $ map (\df -> df.platformFee.cgst) driverFees
      sgstAmount = sum $ map (\df -> df.platformFee.sgst) driverFees
      gstCollected = cgstAmount + sgstAmount
      govtCharges = sum $ map (\df -> df.govtCharges) driverFees
      -- Subscription charges from MANDATE_REGISTRATION and RECURRING_INVOICE fees
      subscriptionCharges = sum $ map (\df -> df.platformFee.fee) $
        filter (\df -> df.feeType == DDF.MANDATE_REGISTRATION || df.feeType == DDF.RECURRING_INVOICE) driverFees
      -- Penalties from cancellation penalty amounts
      penalties = sum $ map (\df -> fromMaybe 0 df.cancellationPenaltyAmount) driverFees
      -- Incentives (bonus earnings)
      incentives = sum $ map (\df -> df.specialZoneAmount) driverFees
      tollReimbursements = 0  -- Toll data would come from ride-level queries when available
      tdsDeducted = govtCharges
      platformFees = platformFeeAmount
      -- Net earnings = gross - platform fees - subscriptions + incentives - penalties - GST - TDS
      netEarnings = grossEarnings - platformFees - subscriptionCharges + incentives - penalties + tollReimbursements - gstCollected - tdsDeducted
      totalPayouts = 0  -- Payout data aggregation would require payout transaction records
      openingBalance = 0  -- Would be the closingBalance of the previous statement
      closingBalance = openingBalance + netEarnings - totalPayouts
      currency_ = INR

  -- C2 fix: Compute SHA256 verification hash over the financial fields
  let hashInput = computeStatementHashInput
        grossEarnings platformFees subscriptionCharges incentives
        penalties tollReimbursements gstCollected tdsDeducted
        netEarnings totalPayouts openingBalance closingBalance
        req.periodStart req.periodEnd driverId
      verificationHash = computeSHA256Hash hashInput

  let statement =
        DDFS.DriverFinancialStatement
          { id = statementId,
            driverId = driverId,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOpCityId,
            referenceNumber = refNum,
            periodType = req.periodType,
            periodStart = req.periodStart,
            periodEnd = req.periodEnd,
            grossEarnings = grossEarnings,
            platformFees = platformFees,
            subscriptionCharges = subscriptionCharges,
            incentives = incentives,
            penalties = penalties,
            tollReimbursements = tollReimbursements,
            gstCollected = gstCollected,
            tdsDeducted = tdsDeducted,
            netEarnings = netEarnings,
            totalPayouts = totalPayouts,
            openingBalance = openingBalance,
            closingBalance = closingBalance,
            currency = currency_,
            pdfUrl = Nothing,
            verificationHash = verificationHash,
            status = DDFS.READY,
            createdAt = now,
            updatedAt = now
          }
  QDFS.create statement
  pure statement

-- | Download the PDF for a given statement.
downloadStatement ::
  ( Maybe (Id DP.Person),
    Id DM.Merchant,
    Id DMOC.MerchantOperatingCity
  ) ->
  Id DDFS.DriverFinancialStatement ->
  Flow StatementDownloadRes
downloadStatement (mbDriverId, _merchantId, _merchantOpCityId) statementId = do
  driverId <- mbDriverId & fromMaybeM (PersonNotFound "No person found")
  statement <- QDFS.findById statementId >>= fromMaybeM (InvalidRequest "Statement not found")
  unless (statement.driverId == driverId) $
    throwError (InvalidRequest "Statement does not belong to this driver")
  pure $
    StatementDownloadRes
      { statementId = statement.id,
        pdfUrl = statement.pdfUrl,
        status = statement.status
      }

-- | Get a summary view of a statement without triggering a download.
getStatementSummary ::
  ( Maybe (Id DP.Person),
    Id DM.Merchant,
    Id DMOC.MerchantOperatingCity
  ) ->
  Id DDFS.DriverFinancialStatement ->
  Flow StatementSummaryRes
getStatementSummary (mbDriverId, _merchantId, _merchantOpCityId) statementId = do
  driverId <- mbDriverId & fromMaybeM (PersonNotFound "No person found")
  statement <- QDFS.findById statementId >>= fromMaybeM (InvalidRequest "Statement not found")
  unless (statement.driverId == driverId) $
    throwError (InvalidRequest "Statement does not belong to this driver")
  pure $
    StatementSummaryRes
      { statementId = statement.id,
        referenceNumber = statement.referenceNumber,
        periodType = statement.periodType,
        periodStart = statement.periodStart,
        periodEnd = statement.periodEnd,
        grossEarnings = statement.grossEarnings,
        platformFees = statement.platformFees,
        subscriptionCharges = statement.subscriptionCharges,
        incentives = statement.incentives,
        penalties = statement.penalties,
        tollReimbursements = statement.tollReimbursements,
        gstCollected = statement.gstCollected,
        tdsDeducted = statement.tdsDeducted,
        netEarnings = statement.netEarnings,
        totalPayouts = statement.totalPayouts,
        openingBalance = statement.openingBalance,
        closingBalance = statement.closingBalance,
        currency = statement.currency,
        status = statement.status
      }

-- | Generate a unique reference number for a statement.
-- Format: NY-STM-YYYYMMDD-<short driver id suffix>-<random suffix>
generateReferenceNumber :: Id DP.Person -> UTCTime -> Flow Text
generateReferenceNumber driverId now = do
  suffix <- generateGUID
  let dateStr = T.pack . show . utctDay $ now
      driverSuffix = T.takeEnd 6 driverId.getId
      shortSuffix = T.takeEnd 8 suffix.getId
  pure $ "NY-STM-" <> dateStr <> "-" <> driverSuffix <> "-" <> shortSuffix

-- | Compute a SHA256 hash over the statement's financial fields for tamper detection.
computeStatementHashInput ::
  HighPrecMoney -> HighPrecMoney -> HighPrecMoney -> HighPrecMoney ->
  HighPrecMoney -> HighPrecMoney -> HighPrecMoney -> HighPrecMoney ->
  HighPrecMoney -> HighPrecMoney -> HighPrecMoney -> HighPrecMoney ->
  UTCTime -> UTCTime -> Id DP.Person -> Text
computeStatementHashInput
  grossEarnings platformFees subscriptionCharges incentives
  penalties tollReimbursements gstCollected tdsDeducted
  netEarnings totalPayouts openingBalance closingBalance
  periodStart periodEnd driverId =
    T.intercalate "|"
      [ show grossEarnings, show platformFees, show subscriptionCharges, show incentives,
        show penalties, show tollReimbursements, show gstCollected, show tdsDeducted,
        show netEarnings, show totalPayouts, show openingBalance, show closingBalance,
        show periodStart, show periodEnd, driverId.getId
      ]

-- | Compute SHA256 hash of a text input and return hex-encoded result.
computeSHA256Hash :: Text -> Text
computeSHA256Hash input =
  let bs = TE.encodeUtf8 input
      digest = Hash.hash bs :: Hash.Digest Hash.SHA256
      hexBytes = BA.unpack digest
  in T.pack $ concatMap (\b -> let h = showHex' b in h) hexBytes
  where
    showHex' :: Word8 -> String
    showHex' w =
      let (hi, lo) = w `divMod` 16
      in [hexChar hi, hexChar lo]
    hexChar :: Word8 -> Char
    hexChar n
      | n < 10 = toEnum (fromEnum '0' + fromIntegral n)
      | otherwise = toEnum (fromEnum 'a' + fromIntegral n - 10)
