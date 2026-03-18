module Domain.Action.Dashboard.Operator.Invoice
  ( getInvoiceListInvoices,
    getInvoiceGetInvoiceDetail,
    getInvoiceDownloadInvoice,
    postInvoiceBulkDownloadInvoices,
    postInvoiceUploadInvoice,
  )
where

import qualified API.Types.ProviderPlatform.Operator.Endpoints.Invoice as InvoiceEndpoints
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.OperatorInvoice as DOI
import qualified Domain.Types.Person as DP
import Environment
import EulerHS.Prelude (whenNothing_)
import Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.Queries.OperatorInvoiceExtra as QOI

-- | List operator invoices with filters
getInvoiceListInvoices ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe Text -> -- requestorId
  Maybe UTCTime -> -- from
  Maybe UTCTime -> -- to
  Maybe Text -> -- invoiceType
  Maybe Text -> -- status
  Maybe Text -> -- fleetOwnerId
  Maybe Int -> -- limit
  Maybe Int -> -- offset
  Flow InvoiceEndpoints.OperatorInvoiceListRes
getInvoiceListInvoices merchantShortId _opCity mbRequestorId mbFrom mbTo mbInvoiceType mbStatus mbFleetOwnerId mbLimit mbOffset = do
  merchant <- findMerchantByShortId merchantShortId
  let requestorId = fromMaybe "" mbRequestorId
  let limitVal = min 50 (fromMaybe 10 mbLimit)
  let offsetVal = fromMaybe 0 mbOffset
  let mbFleetOwnerPersonId = Id <$> mbFleetOwnerId :: Maybe (Id DP.Person)
  let mbInvoiceTypeEnum = mbInvoiceType >>= readMaybe . T.unpack :: Maybe DOI.OperatorInvoiceType
  let mbStatusEnum = mbStatus >>= readMaybe . T.unpack :: Maybe DOI.OperatorInvoiceStatus
  invoices <- QOI.findAllByOperatorWithFilters
    (Id requestorId)
    merchant.id
    mbFrom
    mbTo
    mbInvoiceTypeEnum
    mbStatusEnum
    mbFleetOwnerPersonId
    limitVal
    offsetVal
  totalCount <- QOI.countByOperatorWithFilters
    (Id requestorId)
    merchant.id
    mbFrom
    mbTo
    mbInvoiceTypeEnum
    mbStatusEnum
    mbFleetOwnerPersonId
  let items = map toListItem invoices
  pure $
    InvoiceEndpoints.OperatorInvoiceListRes
      { invoices = items,
        totalItems = totalCount,
        summary = Dashboard.Common.Summary {totalCount = totalCount, count = length items}
      }

-- | Get operator invoice detail
getInvoiceGetInvoiceDetail ::
  ShortId DM.Merchant ->
  Context.City ->
  Text -> -- invoiceId
  Maybe Text -> -- requestorId
  Flow InvoiceEndpoints.OperatorInvoiceDetail
getInvoiceGetInvoiceDetail merchantShortId _opCity invoiceId _mbRequestorId = do
  _merchant <- findMerchantByShortId merchantShortId
  invoice <- QOI.findById (Id invoiceId) >>= fromMaybeM (InvalidRequest "Invoice not found")
  pure $ toDetail invoice

-- | Download a single invoice (returns pre-signed S3 URL)
getInvoiceDownloadInvoice ::
  ShortId DM.Merchant ->
  Context.City ->
  Text -> -- invoiceId
  Maybe Text -> -- requestorId
  Flow Text
getInvoiceDownloadInvoice merchantShortId _opCity invoiceId _mbRequestorId = do
  _merchant <- findMerchantByShortId merchantShortId
  invoice <- QOI.findById (Id invoiceId) >>= fromMaybeM (InvalidRequest "Invoice not found")
  -- Return the S3 key; in production this would generate a pre-signed URL
  pure invoice.s3Key

-- | Bulk download invoices (returns URL to zip archive)
postInvoiceBulkDownloadInvoices ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe Text -> -- requestorId
  InvoiceEndpoints.BulkDownloadReq ->
  Flow InvoiceEndpoints.BulkDownloadRes
postInvoiceBulkDownloadInvoices merchantShortId _opCity _mbRequestorId req = do
  _merchant <- findMerchantByShortId merchantShortId
  -- In production, this would:
  -- 1. Fetch all invoice S3 keys
  -- 2. Create a zip archive
  -- 3. Upload zip to S3
  -- 4. Return pre-signed URL
  -- For now, return a placeholder
  let invoiceCount = length req.invoiceIds
  when (invoiceCount == 0) $ throwError (InvalidRequest "No invoice IDs provided")
  when (invoiceCount > 100) $ throwError (InvalidRequest "Maximum 100 invoices per bulk download")
  pure $ InvoiceEndpoints.BulkDownloadRes
    { downloadUrl = "https://s3.amazonaws.com/placeholder/bulk-download.zip"
    }

-- | Upload an invoice
postInvoiceUploadInvoice ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe Text -> -- requestorId
  InvoiceEndpoints.InvoiceUploadReq ->
  Flow InvoiceEndpoints.InvoiceUploadRes
postInvoiceUploadInvoice merchantShortId opCity mbRequestorId req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let requestorId = fromMaybe "" mbRequestorId
  newId <- generateGUID
  now <- getCurrentTime

  -- Validate file name
  when (T.null req.fileName) $ throwError (InvalidRequest "File name is required")

  -- Build S3 key
  let s3Key = "operator-invoices/" <> requestorId <> "/" <> T.pack (show (utctDay now)) <> "/" <> newId.getId <> ".pdf"

  -- In production, decode base64 and upload to S3 here
  -- For now, we just store the metadata

  let mbInvoiceType = readMaybe (T.unpack req.invoiceType) :: Maybe DOI.OperatorInvoiceType
  invoiceType <- fromMaybeM (InvalidRequest "Invalid invoice type") (pure mbInvoiceType)

  let invoice = DOI.OperatorInvoice
        { id = Id newId.getId,
          operatorId = Id requestorId,
          fleetOwnerId = Id <$> req.fleetOwnerId,
          invoiceType = invoiceType,
          invoiceNumber = Nothing,
          invoiceDate = req.invoiceDate,
          amount = req.amount,
          currency = Kernel.Types.Common.INR,
          fileName = req.fileName,
          s3Key = s3Key,
          remarks = req.remarks,
          status = DOI.ACTIVE,
          merchantId = merchant.id,
          merchantOperatingCityId = merchantOpCityId,
          createdAt = now,
          updatedAt = now
        }

  QOI.create invoice
  pure $ InvoiceEndpoints.InvoiceUploadRes {invoiceId = newId.getId}

-- | Convert domain invoice to list item
toListItem :: DOI.OperatorInvoice -> InvoiceEndpoints.OperatorInvoiceListItem
toListItem inv =
  InvoiceEndpoints.OperatorInvoiceListItem
    { invoiceId = inv.id.getId,
      invoiceNumber = inv.invoiceNumber,
      invoiceDate = inv.invoiceDate,
      invoiceType = show inv.invoiceType,
      source = case inv.invoiceType of
        DOI.PLATFORM_INVOICE -> "PLATFORM"
        _ -> "UPLOADED",
      fleetOwnerName = Nothing, -- Would be resolved via Person lookup in production
      amount = inv.amount,
      currency = inv.currency,
      status = show inv.status,
      hasDownload = True
    }

-- | Convert domain invoice to detail
toDetail :: DOI.OperatorInvoice -> InvoiceEndpoints.OperatorInvoiceDetail
toDetail inv =
  InvoiceEndpoints.OperatorInvoiceDetail
    { invoiceId = inv.id.getId,
      invoiceNumber = inv.invoiceNumber,
      invoiceDate = inv.invoiceDate,
      invoiceType = show inv.invoiceType,
      source = case inv.invoiceType of
        DOI.PLATFORM_INVOICE -> "PLATFORM"
        _ -> "UPLOADED",
      fleetOwnerId = (.getId) <$> inv.fleetOwnerId,
      fleetOwnerName = Nothing, -- Would be resolved via Person lookup in production
      amount = inv.amount,
      currency = inv.currency,
      taxDetails = Nothing, -- Would be populated for platform invoices
      downloadUrl = Just inv.s3Key, -- In production, generate pre-signed URL
      remarks = inv.remarks,
      status = show inv.status,
      createdAt = inv.createdAt
    }
