{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Invoice where

import Data.Time (UTCTime (UTCTime, utctDay), secondsToDiffTime)
import qualified Domain.Types.DriverFee as DF
import qualified Domain.Types.Invoice as Domain
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import Domain.Types.Person (Person)
import Domain.Types.Plan (ServiceNames (YATRI_SUBSCRIPTION))
import Kernel.Beam.Functions (createWithKV, findAllWithKV, findAllWithOptionsKV, findAllWithOptionsKV', findOneWithKV, updateWithKV)
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Invoice as BeamI hiding (Id)
import qualified Storage.Queries.DriverFee as QDF

create :: KvDbFlow m r => Domain.Invoice -> m ()
create = createWithKV

findById :: KvDbFlow m r => Id Domain.Invoice -> m [Domain.Invoice]
findById (Id invoiceId) = findAllWithKV [Se.Is BeamI.id $ Se.Eq invoiceId]

checkIfAutoPayInvoiceById :: KvDbFlow m r => Id Domain.Invoice -> m Bool
checkIfAutoPayInvoiceById (Id invoiceId) = do
  invoices <-
    findAllWithKV
      [ Se.And
          [ Se.Is BeamI.id $ Se.Eq invoiceId,
            Se.Is BeamI.paymentMode $ Se.Eq Domain.AUTOPAY_INVOICE
          ]
      ]
  return $ not (null invoices)

createMany :: KvDbFlow m r => [Domain.Invoice] -> m ()
createMany = traverse_ create

findValidByDriverFeeId :: KvDbFlow m r => Id DF.DriverFee -> m [Domain.Invoice]
findValidByDriverFeeId (Id driverFeeId) =
  findAllWithKV
    [ Se.And
        [ Se.Is BeamI.driverFeeId $ Se.Eq driverFeeId,
          Se.Is BeamI.invoiceStatus $ Se.Not $ Se.In [Domain.INACTIVE, Domain.EXPIRED]
        ]
    ]

findAllInvoicesByDriverIdWithLimitAndOffset ::
  KvDbFlow m r =>
  Id Person ->
  [Domain.InvoicePaymentMode] ->
  Int ->
  Int ->
  ServiceNames ->
  m [Domain.Invoice]
findAllInvoicesByDriverIdWithLimitAndOffset driverId paymentModes limit offset serviceName = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is BeamI.driverId $ Se.Eq (driverId.getId),
          Se.Is BeamI.paymentMode $ Se.In paymentModes,
          Se.Is BeamI.invoiceStatus $ Se.Not $ Se.In [Domain.INACTIVE, Domain.EXPIRED],
          Se.Is BeamI.serviceName $ Se.Eq (Just serviceName)
        ]
    ]
    (Se.Desc BeamI.createdAt)
    (Just limit)
    (Just offset)

findLatestByDriverFeeId :: KvDbFlow m r => Id DF.DriverFee -> m (Maybe Domain.Invoice)
findLatestByDriverFeeId (Id driverFeeId) =
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is BeamI.driverFeeId $ Se.Eq driverFeeId,
          Se.Is BeamI.invoiceStatus $ Se.Not $ Se.In [Domain.INACTIVE, Domain.EXPIRED]
        ]
    ]
    (Se.Desc BeamI.createdAt)
    (Just 1)
    Nothing
    <&> listToMaybe

findAllByInvoiceId :: KvDbFlow m r => Id Domain.Invoice -> m [Domain.Invoice]
findAllByInvoiceId (Id invoiceId) = findAllWithKV [Se.Is BeamI.id $ Se.Eq invoiceId]

findAllByInvoiceShortId :: KvDbFlow m r => Text -> m [Domain.Invoice]
findAllByInvoiceShortId invoiceShortId = findAllWithKV [Se.Is BeamI.invoiceShortId $ Se.Eq invoiceShortId]

findByIdWithPaymenModeAndStatus :: KvDbFlow m r => Id Domain.Invoice -> Domain.InvoicePaymentMode -> Domain.InvoiceStatus -> m (Maybe Domain.Invoice)
findByIdWithPaymenModeAndStatus invocieId paymentMode status =
  findOneWithKV
    [ Se.And
        [ Se.Is BeamI.id $ Se.Eq invocieId.getId,
          Se.Is BeamI.invoiceStatus $ Se.Eq status,
          Se.Is BeamI.paymentMode $ Se.Eq paymentMode
        ]
    ]

findActiveManualOrMandateSetupInvoiceByFeeId :: KvDbFlow m r => Id DF.DriverFee -> m [Domain.Invoice]
findActiveManualOrMandateSetupInvoiceByFeeId (Id driverFeeId) =
  findAllWithKV
    [ Se.And
        [ Se.Is BeamI.driverFeeId $ Se.Eq driverFeeId,
          Se.Is BeamI.invoiceStatus $ Se.Eq Domain.ACTIVE_INVOICE,
          Se.Is BeamI.paymentMode $ Se.In [Domain.MANUAL_INVOICE, Domain.MANDATE_SETUP_INVOICE]
        ]
    ]

findActiveManualInvoiceByFeeId :: KvDbFlow m r => Id DF.DriverFee -> m [Domain.Invoice]
findActiveManualInvoiceByFeeId (Id driverFeeId) =
  findAllWithKV
    [ Se.And
        [ Se.Is BeamI.driverFeeId $ Se.Eq driverFeeId,
          Se.Is BeamI.invoiceStatus $ Se.Eq Domain.ACTIVE_INVOICE,
          Se.Is BeamI.paymentMode $ Se.Eq Domain.MANUAL_INVOICE
        ]
    ]

findActiveMandateSetupInvoiceByFeeId :: KvDbFlow m r => Id DF.DriverFee -> m [Domain.Invoice]
findActiveMandateSetupInvoiceByFeeId (Id driverFeeId) =
  findAllWithKV
    [ Se.And
        [ Se.Is BeamI.driverFeeId $ Se.Eq driverFeeId,
          Se.Is BeamI.invoiceStatus $ Se.Eq Domain.ACTIVE_INVOICE,
          Se.Is BeamI.paymentMode $ Se.Eq Domain.MANDATE_SETUP_INVOICE
        ]
    ]

findByDriverFeeIds :: KvDbFlow m r => [Id DF.DriverFee] -> m [Domain.Invoice]
findByDriverFeeIds driverFeeIds =
  findAllWithKV
    [Se.Is BeamI.driverFeeId $ Se.In (getId <$> driverFeeIds)]

findActiveByDriverFeeIds :: KvDbFlow m r => [Id DF.DriverFee] -> m [Domain.Invoice]
findActiveByDriverFeeIds driverFeeIds =
  findAllWithKV
    [ Se.And
        [ Se.Is BeamI.driverFeeId $ Se.In (getId <$> driverFeeIds),
          Se.Is BeamI.invoiceStatus $ Se.Eq Domain.ACTIVE_INVOICE
        ]
    ]

findAllByStatusWithLimit ::
  KvDbFlow m r =>
  Domain.InvoiceStatus ->
  Id DMOC.MerchantOperatingCity ->
  Int ->
  m [Domain.Invoice]
findAllByStatusWithLimit status merchantOperatingCityId limit = do
  endTime <- getCurrentTime
  let startTime = addUTCTime (-1 * 3 * 3600 * 24) endTime
  let lastCheckedAt = UTCTime (utctDay endTime) (secondsToDiffTime 0)
  findAllWithOptionsKV'
    [ Se.And
        [ Se.Is BeamI.invoiceStatus $ Se.Eq status,
          Se.Is BeamI.createdAt $ Se.LessThanOrEq endTime,
          Se.Is BeamI.createdAt $ Se.GreaterThanOrEq startTime,
          Se.Is BeamI.merchantOperatingCityId $ Se.Eq $ Just merchantOperatingCityId.getId,
          Se.Or
            [ Se.Is BeamI.lastStatusCheckedAt $ Se.Eq Nothing,
              Se.Is BeamI.lastStatusCheckedAt $ Se.Not (Se.Eq $ Just lastCheckedAt)
            ]
        ]
    ]
    (Just limit)
    Nothing

findAllAutoPayInvoicesActiveOlderThanProvidedDuration ::
  KvDbFlow m r =>
  NominalDiffTime ->
  Id DMOC.MerchantOperatingCity ->
  m [Domain.Invoice]
findAllAutoPayInvoicesActiveOlderThanProvidedDuration timeDiff merchantOperatingCityId = do
  endTime <- getCurrentTime
  let startTime = addUTCTime (-1 * timeDiff) endTime
  findAllWithKV
    [ Se.And
        [ Se.Is BeamI.invoiceStatus $ Se.Eq Domain.ACTIVE_INVOICE,
          Se.Is BeamI.paymentMode $ Se.Eq Domain.AUTOPAY_INVOICE,
          Se.Is BeamI.merchantOperatingCityId $ Se.Eq (Just $ getId merchantOperatingCityId),
          Se.Is BeamI.createdAt $ Se.LessThan startTime
        ]
    ]

findLatestAutopayActiveByDriverFeeId :: KvDbFlow m r => Id DF.DriverFee -> m [Domain.Invoice]
findLatestAutopayActiveByDriverFeeId driverFeeId = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is BeamI.driverFeeId $ Se.Eq (getId driverFeeId),
          Se.Is BeamI.invoiceStatus $ Se.Eq Domain.ACTIVE_INVOICE,
          Se.Is BeamI.paymentMode $ Se.Eq Domain.AUTOPAY_INVOICE
        ]
    ]
    (Se.Desc BeamI.createdAt)
    (Just 1)
    Nothing

findLatestNonAutopayActiveByDriverId :: KvDbFlow m r => Id Person -> ServiceNames -> m [Domain.Invoice]
findLatestNonAutopayActiveByDriverId driverId serviceName = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is BeamI.driverId $ Se.Eq (getId driverId),
          Se.Is BeamI.paymentMode $ Se.Not $ Se.Eq Domain.AUTOPAY_INVOICE,
          Se.Is BeamI.serviceName $ Se.Eq (Just serviceName)
        ]
    ]
    (Se.Desc BeamI.createdAt)
    (Just 1)
    Nothing

updateInvoiceStatusByInvoiceId :: KvDbFlow m r => Domain.InvoiceStatus -> Id Domain.Invoice -> m ()
updateInvoiceStatusByInvoiceId invoiceStatus invoiceId = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamI.invoiceStatus invoiceStatus,
      Se.Set BeamI.updatedAt now
    ]
    [ Se.And
        ( [Se.Is BeamI.id (Se.Eq $ getId invoiceId)]
            <> [Se.Is BeamI.invoiceStatus $ Se.Eq Domain.ACTIVE_INVOICE | invoiceStatus == Domain.INACTIVE]
        )
    ]

updateInvoiceStatusByDriverFeeIdsAndMbPaymentMode :: KvDbFlow m r => Domain.InvoiceStatus -> [Id DF.DriverFee] -> Maybe Domain.InvoicePaymentMode -> m ()
updateInvoiceStatusByDriverFeeIdsAndMbPaymentMode status driverFeeIds paymentMode = do
  now <- getCurrentTime
  let paymentModeCheck =
        case paymentMode of
          Just paymentMode' -> [Se.Is BeamI.paymentMode $ Se.Eq paymentMode']
          Nothing -> []
  updateWithKV
    [ Se.Set BeamI.invoiceStatus status,
      Se.Set BeamI.updatedAt now
    ]
    ( [Se.Is BeamI.driverFeeId $ Se.In (getId <$> driverFeeIds)]
        <> paymentModeCheck
    )

updateStatusAndTypeByMbdriverFeeIdAndInvoiceId :: KvDbFlow m r => Id Domain.Invoice -> Maybe Domain.InvoiceStatus -> Maybe Domain.InvoicePaymentMode -> Maybe (Id DF.DriverFee) -> m ()
updateStatusAndTypeByMbdriverFeeIdAndInvoiceId invoiceId status paymentMode driverFeeId = do
  now <- getCurrentTime
  updateWithKV
    ( [Se.Set BeamI.updatedAt now]
        <> [Se.Set BeamI.invoiceStatus (fromJust status) | isJust status]
        <> [Se.Set BeamI.paymentMode (fromJust paymentMode) | isJust paymentMode]
    )
    ([Se.Is BeamI.driverFeeId $ Se.Eq (getId (fromJust driverFeeId)) | isJust driverFeeId] <> [Se.Is BeamI.id $ Se.Eq invoiceId.getId])

updatePendingToFailed ::
  KvDbFlow m r =>
  NominalDiffTime ->
  Id DMOC.MerchantOperatingCity ->
  m ()
updatePendingToFailed seconds merchantOperatingCityId = do
  endTime <- getCurrentTime
  let startTime = addUTCTime (-1 * seconds) endTime
  updateWithKV
    [Se.Set BeamI.invoiceStatus Domain.INACTIVE]
    [ Se.And
        [ Se.Is BeamI.invoiceStatus $ Se.Eq Domain.ACTIVE_INVOICE,
          Se.Is BeamI.merchantOperatingCityId $ Se.Eq (Just merchantOperatingCityId.getId),
          Se.Is BeamI.createdAt $ Se.LessThan startTime
        ]
    ]

inActivateAllAutopayActiveInvoices :: KvDbFlow m r => Id Person -> ServiceNames -> m ()
inActivateAllAutopayActiveInvoices driverId serviceName = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamI.invoiceStatus Domain.INACTIVE,
      Se.Set BeamI.updatedAt now
    ]
    [ Se.And
        [ Se.Is BeamI.driverId $ Se.Eq (getId driverId),
          Se.Is BeamI.invoiceStatus $ Se.Eq Domain.ACTIVE_INVOICE,
          Se.Is BeamI.paymentMode $ Se.Eq Domain.AUTOPAY_INVOICE,
          Se.Is BeamI.serviceName $ Se.Eq (Just serviceName)
        ]
    ]

updateBankErrorsByInvoiceId :: KvDbFlow m r => Maybe Text -> Maybe Text -> Id Domain.Invoice -> m ()
updateBankErrorsByInvoiceId bankError errorCode invoiceId = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamI.bankErrorMessage bankError,
      Se.Set BeamI.bankErrorCode errorCode,
      Se.Set BeamI.bankErrorUpdatedAt (Just now)
    ]
    [Se.Is BeamI.id (Se.Eq $ getId invoiceId)]

updateLastCheckedOn :: KvDbFlow m r => [Id Domain.Invoice] -> m ()
updateLastCheckedOn invoiceIds = do
  now <- getCurrentTime
  let lastCheckedAt = UTCTime (utctDay now) (secondsToDiffTime 0)
  updateWithKV
    [ Se.Set BeamI.lastStatusCheckedAt (Just lastCheckedAt),
      Se.Set BeamI.updatedAt now
    ]
    [Se.Is BeamI.id (Se.In $ getId <$> invoiceIds)]

updateMerchantOperatingCityIdByInvoiceId :: KvDbFlow m r => Id Domain.Invoice -> Id DMOC.MerchantOperatingCity -> m ()
updateMerchantOperatingCityIdByInvoiceId invoiceId merchantOperatingCityId = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamI.merchantOperatingCityId (Just $ getId merchantOperatingCityId),
      Se.Set BeamI.updatedAt now
    ]
    [Se.Is BeamI.id (Se.Eq $ getId invoiceId)]

instance FromTType' BeamI.Invoice Domain.Invoice where
  fromTType' BeamI.InvoiceT {..} = do
    merchantOperatingCityId' <- case merchantOperatingCityId of
      Just opCity -> return $ Id opCity
      Nothing -> do
        dfee <- QDF.findById (Id driverFeeId) >>= fromMaybeM (DriverFeeNotFound driverFeeId)
        updateMerchantOperatingCityIdByInvoiceId (Id id) (dfee.merchantOperatingCityId)
        return dfee.merchantOperatingCityId
    pure $
      Just
        Domain.Invoice
          { id = Id id,
            invoiceShortId = invoiceShortId,
            driverId = Id driverId,
            driverFeeId = Id driverFeeId,
            invoiceStatus = invoiceStatus,
            paymentMode = paymentMode,
            maxMandateAmount = maxMandateAmount,
            bankErrorCode,
            bankErrorMessage,
            bankErrorUpdatedAt,
            lastStatusCheckedAt,
            merchantOperatingCityId = merchantOperatingCityId',
            Domain.serviceName = fromMaybe YATRI_SUBSCRIPTION serviceName,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' BeamI.Invoice Domain.Invoice where
  toTType' Domain.Invoice {..} = do
    BeamI.InvoiceT
      { BeamI.id = id.getId,
        BeamI.invoiceShortId = invoiceShortId,
        BeamI.driverFeeId = getId driverFeeId,
        BeamI.driverId = getId driverId,
        BeamI.paymentMode = paymentMode,
        BeamI.invoiceStatus = invoiceStatus,
        BeamI.maxMandateAmount = maxMandateAmount,
        BeamI.bankErrorCode = bankErrorCode,
        BeamI.bankErrorMessage = bankErrorMessage,
        BeamI.bankErrorUpdatedAt = bankErrorUpdatedAt,
        BeamI.lastStatusCheckedAt = lastStatusCheckedAt,
        BeamI.serviceName = Just serviceName,
        BeamI.merchantOperatingCityId = Just merchantOperatingCityId.getId,
        BeamI.createdAt = createdAt,
        BeamI.updatedAt = updatedAt
      }
