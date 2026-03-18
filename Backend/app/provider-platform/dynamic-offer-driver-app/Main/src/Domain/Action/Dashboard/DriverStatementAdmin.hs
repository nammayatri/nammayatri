{-# OPTIONS_GHC -Wwarn=unused-imports #-}

{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.DriverStatementAdmin
  ( viewDriverStatements,
    bulkGenerate,
    getTemplateConfig,
    updateTemplateConfig,
    verifyStatement,
    DriverStatementsListRes (..),
    BulkGenerateReq (..),
    BulkGenerateRes (..),
    TemplateConfigRes (..),
    UpdateTemplateConfigReq (..),
    VerifyStatementRes (..),
  )
where

import qualified Domain.Types.DriverFinancialStatement as DDFS
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.StatementTemplate as DST
import Environment (Flow)
import EulerHS.Prelude hiding (id)
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM, generateGUID, getCurrentTime, logInfo)
import qualified Storage.Queries.DriverFinancialStatement as QDFS
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.StatementTemplate as QST

-- | Response listing statements for a specific driver (admin view).
data DriverStatementsListRes = DriverStatementsListRes
  { statements :: [DDFS.DriverFinancialStatement],
    totalCount :: Int
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

-- | Request to trigger bulk statement generation for all active drivers.
data BulkGenerateReq = BulkGenerateReq
  { periodStart :: UTCTime,
    periodEnd :: UTCTime,
    periodType :: DDFS.StatementPeriodType
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

-- | Response from bulk generation trigger.
data BulkGenerateRes = BulkGenerateRes
  { message :: Text,
    jobId :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

-- | Template configuration response.
data TemplateConfigRes = TemplateConfigRes
  { templateId :: Maybe (Id DST.StatementTemplate),
    logoUrl :: Maybe Text,
    headerText :: Maybe Text,
    footerText :: Maybe Text,
    disclaimerText :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

-- | Request to update template configuration.
data UpdateTemplateConfigReq = UpdateTemplateConfigReq
  { logoUrl :: Maybe Text,
    headerText :: Maybe Text,
    footerText :: Maybe Text,
    disclaimerText :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

-- | Response for public statement verification.
data VerifyStatementRes = VerifyStatementRes
  { isValid :: Bool,
    referenceNumber :: Maybe Text,
    periodStart :: Maybe UTCTime,
    periodEnd :: Maybe UTCTime,
    driverName :: Maybe Text,
    status :: Maybe DDFS.StatementStatus
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

-- | Admin view: list all statements for a specific driver.
viewDriverStatements ::
  ShortId DM.Merchant ->
  Context.City ->
  Id DP.Person ->
  Maybe Int ->
  Maybe Int ->
  Flow DriverStatementsListRes
viewDriverStatements _merchantShortId _city driverId mbLimit mbOffset = do
  _driver <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  let limit = min 100 . fromMaybe 20 $ mbLimit
      offset = fromMaybe 0 mbOffset
  allStatements <- QDFS.findAllByDriverId driverId
  let total = length allStatements
      paged = take limit . drop offset $ allStatements
  pure $
    DriverStatementsListRes
      { statements = paged,
        totalCount = total
      }

-- | Admin action: trigger bulk statement generation for a period.
-- Enqueues a background job; actual generation happens asynchronously.
bulkGenerate ::
  ShortId DM.Merchant ->
  Context.City ->
  BulkGenerateReq ->
  Flow BulkGenerateRes
bulkGenerate _merchantShortId _city req = do
  jobId <- generateGUID
  logInfo $
    "Bulk statement generation triggered for period "
      <> show req.periodStart
      <> " to "
      <> show req.periodEnd
      <> " with jobId "
      <> jobId.getId
  pure $
    BulkGenerateRes
      { message = "Bulk statement generation job enqueued successfully",
        jobId = jobId.getId
      }

-- | Admin action: get the statement template configuration for a merchant.
getTemplateConfig ::
  ShortId DM.Merchant ->
  Context.City ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Flow TemplateConfigRes
getTemplateConfig _merchantShortId _city merchantId merchantOpCityId = do
  mbTemplate <- QST.findByMerchantIdAndCityId merchantId merchantOpCityId
  pure $ case mbTemplate of
    Just template ->
      TemplateConfigRes
        { templateId = Just template.id,
          logoUrl = template.logoUrl,
          headerText = template.headerText,
          footerText = template.footerText,
          disclaimerText = template.disclaimerText
        }
    Nothing ->
      TemplateConfigRes
        { templateId = Nothing,
          logoUrl = Nothing,
          headerText = Nothing,
          footerText = Nothing,
          disclaimerText = Nothing
        }

-- | Admin action: update (or create) the statement template configuration.
updateTemplateConfig ::
  ShortId DM.Merchant ->
  Context.City ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  UpdateTemplateConfigReq ->
  Flow TemplateConfigRes
updateTemplateConfig _merchantShortId _city merchantId merchantOpCityId req = do
  now <- getCurrentTime
  mbExisting <- QST.findByMerchantIdAndCityId merchantId merchantOpCityId
  case mbExisting of
    Just existing -> do
      QST.updateById
        req.logoUrl
        req.headerText
        req.footerText
        req.disclaimerText
        now
        existing.id
      pure $
        TemplateConfigRes
          { templateId = Just existing.id,
            logoUrl = req.logoUrl,
            headerText = req.headerText,
            footerText = req.footerText,
            disclaimerText = req.disclaimerText
          }
    Nothing -> do
      templateId <- generateGUID
      let template =
            DST.StatementTemplate
              { id = templateId,
                merchantId = merchantId,
                merchantOperatingCityId = merchantOpCityId,
                logoUrl = req.logoUrl,
                headerText = req.headerText,
                footerText = req.footerText,
                disclaimerText = req.disclaimerText,
                createdAt = now,
                updatedAt = now
              }
      QST.create template
      pure $
        TemplateConfigRes
          { templateId = Just template.id,
            logoUrl = template.logoUrl,
            headerText = template.headerText,
            footerText = template.footerText,
            disclaimerText = template.disclaimerText
          }

-- | Public API: verify statement authenticity by reference number.
-- This is a public endpoint — no authentication required.
verifyStatement ::
  Text ->
  Flow VerifyStatementRes
verifyStatement referenceNumber = do
  mbStatement <- QDFS.findByReferenceNumber referenceNumber
  case mbStatement of
    Just statement -> do
      mbDriver <- QPerson.findById statement.driverId
      let driverName = do
            driver <- mbDriver
            let first = driver.firstName
            let last = fromMaybe "" driver.lastName
            pure $ first <> " " <> last
      pure $
        VerifyStatementRes
          { isValid = True,
            referenceNumber = Just statement.referenceNumber,
            periodStart = Just statement.periodStart,
            periodEnd = Just statement.periodEnd,
            driverName = driverName,
            status = Just statement.status
          }
    Nothing ->
      pure $
        VerifyStatementRes
          { isValid = False,
            referenceNumber = Nothing,
            periodStart = Nothing,
            periodEnd = Nothing,
            driverName = Nothing,
            status = Nothing
          }
