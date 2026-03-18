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

import qualified Data.Text as T
import Data.Time (utctDay)
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
import qualified Storage.Queries.DriverFinancialStatement as QDFS
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
  allStatements <- QDFS.findAllByDriverId driverId
  let total = length allStatements
      paged = take limit . drop offset $ allStatements
  pure $
    StatementListRes
      { statements = paged,
        totalCount = total
      }

-- | Generate a new financial statement for a custom date range.
-- Creates the statement record in GENERATING status. Actual data aggregation
-- and PDF generation would be handled by a background worker or called inline.
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
  now <- getCurrentTime
  statementId <- generateGUID
  refNum <- generateReferenceNumber driverId now

  logInfo $ "Generating financial statement for driver " <> driverId.getId <> " from " <> show req.periodStart <> " to " <> show req.periodEnd

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
            grossEarnings = 0,
            platformFees = 0,
            subscriptionCharges = 0,
            incentives = 0,
            penalties = 0,
            tollReimbursements = 0,
            gstCollected = 0,
            tdsDeducted = 0,
            netEarnings = 0,
            totalPayouts = 0,
            openingBalance = 0,
            closingBalance = 0,
            currency = INR,
            pdfUrl = Nothing,
            verificationHash = "",
            status = DDFS.GENERATING,
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
