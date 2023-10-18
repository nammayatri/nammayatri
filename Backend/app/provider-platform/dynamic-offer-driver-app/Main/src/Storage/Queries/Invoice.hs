{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Invoice where

import Data.Time (UTCTime (UTCTime, utctDay), addUTCTime, secondsToDiffTime)
import Domain.Types.DriverFee (DriverFee)
import Domain.Types.Invoice as Domain
import Domain.Types.Person (Person)
import Kernel.Beam.Functions (FromTType' (fromTType'), ToTType' (toTType'), createWithKV, findAllWithKV, findAllWithOptionsKV, findOneWithKV, updateWithKV)
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import qualified Sequelize as Se
import Storage.Beam.Invoice as BeamI hiding (Id)

create :: MonadFlow m => Domain.Invoice -> m ()
create = createWithKV

findById :: MonadFlow m => Id Domain.Invoice -> m [Domain.Invoice]
findById (Id invoiceId) = findAllWithKV [Se.Is BeamI.id $ Se.Eq invoiceId]

checkIfAutoPayInvoiceById :: MonadFlow m => Id Domain.Invoice -> m Bool
checkIfAutoPayInvoiceById (Id invoiceId) = do
  invoices <-
    findAllWithKV
      [ Se.And
          [ Se.Is BeamI.id $ Se.Eq invoiceId,
            Se.Is BeamI.paymentMode $ Se.Eq AUTOPAY_INVOICE
          ]
      ]
  return $ not (null invoices)

createMany :: MonadFlow m => [Domain.Invoice] -> m ()
createMany = traverse_ create

findValidByDriverFeeId :: MonadFlow m => Id DriverFee -> m [Domain.Invoice]
findValidByDriverFeeId (Id driverFeeId) =
  findAllWithKV
    [ Se.And
        [ Se.Is BeamI.driverFeeId $ Se.Eq driverFeeId,
          Se.Is BeamI.invoiceStatus $ Se.Not $ Se.In [Domain.INACTIVE, Domain.EXPIRED]
        ]
    ]

findAllInvoicesByDriverIdWithLimitAndOffset :: MonadFlow m => Id Person -> [Domain.InvoicePaymentMode] -> Int -> Int -> m [Domain.Invoice]
findAllInvoicesByDriverIdWithLimitAndOffset driverId paymentModes limit offset = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is BeamI.driverId $ Se.Eq (driverId.getId),
          Se.Is BeamI.paymentMode $ Se.In paymentModes,
          Se.Is BeamI.invoiceStatus $ Se.Not $ Se.In [Domain.INACTIVE, Domain.EXPIRED]
        ]
    ]
    (Se.Desc BeamI.createdAt)
    (Just limit)
    (Just offset)

findAllByInvoiceId :: MonadFlow m => Id Domain.Invoice -> m [Domain.Invoice]
findAllByInvoiceId (Id invoiceId) = findAllWithKV [Se.Is BeamI.id $ Se.Eq invoiceId]

findAllByInvoiceShortId :: MonadFlow m => Text -> m [Domain.Invoice]
findAllByInvoiceShortId invoiceShortId = findAllWithKV [Se.Is BeamI.invoiceShortId $ Se.Eq invoiceShortId]

findByIdWithPaymenModeAndStatus :: MonadFlow m => Id Domain.Invoice -> Domain.InvoicePaymentMode -> Domain.InvoiceStatus -> m (Maybe Domain.Invoice)
findByIdWithPaymenModeAndStatus invocieId paymentMode status =
  findOneWithKV
    [ Se.And
        [ Se.Is BeamI.id $ Se.Eq invocieId.getId,
          Se.Is BeamI.invoiceStatus $ Se.Eq status,
          Se.Is BeamI.paymentMode $ Se.Eq paymentMode
        ]
    ]

findActiveManualOrMandateSetupInvoiceByFeeId :: MonadFlow m => Id DriverFee -> m [Domain.Invoice]
findActiveManualOrMandateSetupInvoiceByFeeId (Id driverFeeId) =
  findAllWithKV
    [ Se.And
        [ Se.Is BeamI.driverFeeId $ Se.Eq driverFeeId,
          Se.Is BeamI.invoiceStatus $ Se.Eq Domain.ACTIVE_INVOICE,
          Se.Is BeamI.paymentMode $ Se.In [Domain.MANUAL_INVOICE, Domain.MANDATE_SETUP_INVOICE]
        ]
    ]

findActiveManualInvoiceByFeeId :: MonadFlow m => Id DriverFee -> m [Domain.Invoice]
findActiveManualInvoiceByFeeId (Id driverFeeId) =
  findAllWithKV
    [ Se.And
        [ Se.Is BeamI.driverFeeId $ Se.Eq driverFeeId,
          Se.Is BeamI.invoiceStatus $ Se.Eq Domain.ACTIVE_INVOICE,
          Se.Is BeamI.paymentMode $ Se.Eq Domain.MANUAL_INVOICE
        ]
    ]

findActiveMandateSetupInvoiceByFeeId :: MonadFlow m => Id DriverFee -> m [Domain.Invoice]
findActiveMandateSetupInvoiceByFeeId (Id driverFeeId) =
  findAllWithKV
    [ Se.And
        [ Se.Is BeamI.driverFeeId $ Se.Eq driverFeeId,
          Se.Is BeamI.invoiceStatus $ Se.Eq Domain.ACTIVE_INVOICE,
          Se.Is BeamI.paymentMode $ Se.Eq Domain.MANDATE_SETUP_INVOICE
        ]
    ]

findByDriverFeeIds :: MonadFlow m => [Id DriverFee] -> m [Domain.Invoice]
findByDriverFeeIds driverFeeIds =
  findAllWithKV
    [Se.Is BeamI.driverFeeId $ Se.In (getId <$> driverFeeIds)]

findActiveByDriverFeeIds :: MonadFlow m => [Id DriverFee] -> m [Domain.Invoice]
findActiveByDriverFeeIds driverFeeIds =
  findAllWithKV
    [ Se.And
        [ Se.Is BeamI.driverFeeId $ Se.In (getId <$> driverFeeIds),
          Se.Is BeamI.invoiceStatus $ Se.Eq Domain.ACTIVE_INVOICE
        ]
    ]

findAllByStatusWithLimit :: MonadFlow m => Domain.InvoiceStatus -> Int -> m [Domain.Invoice]
findAllByStatusWithLimit status limit = do
  endTime <- getCurrentTime
  let startTime = addUTCTime (-1 * 3 * 3600 * 24) endTime
  let lastCheckedAt = UTCTime (utctDay endTime) (secondsToDiffTime 0)
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is BeamI.invoiceStatus $ Se.Eq status,
          Se.Is BeamI.createdAt $ Se.LessThanOrEq endTime,
          Se.Is BeamI.createdAt $ Se.GreaterThanOrEq startTime,
          Se.Or
            [ Se.Is BeamI.lastStatusCheckedAt $ Se.Eq Nothing,
              Se.Is BeamI.lastStatusCheckedAt $ Se.Not (Se.Eq $ Just lastCheckedAt)
            ]
        ]
    ]
    (Se.Desc BeamI.createdAt)
    (Just limit)
    Nothing

findAllAutoPayInvoicesActiveOlderThanProvidedDuration :: MonadFlow m => NominalDiffTime -> m [Domain.Invoice]
findAllAutoPayInvoicesActiveOlderThanProvidedDuration timeDiff = do
  endTime <- getCurrentTime
  let startTime = addUTCTime (-1 * timeDiff) endTime
  findAllWithKV
    [ Se.And
        [ Se.Is BeamI.invoiceStatus $ Se.Eq Domain.ACTIVE_INVOICE,
          Se.Is BeamI.paymentMode $ Se.Eq Domain.AUTOPAY_INVOICE,
          Se.Is BeamI.createdAt $ Se.LessThan startTime
        ]
    ]

findLatestAutopayActiveByDriverFeeId :: MonadFlow m => Id DriverFee -> m [Domain.Invoice]
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

findLatestNonAutopayActiveByDriverId :: MonadFlow m => Id Person -> Bool -> m [Domain.Invoice]
findLatestNonAutopayActiveByDriverId driverId isAllowedManual = do
  let allowedTypes = [Domain.MANDATE_SETUP_INVOICE] <> [Domain.MANUAL_INVOICE | isAllowedManual]
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is BeamI.driverId $ Se.Eq (getId driverId),
          Se.Is BeamI.invoiceStatus $ Se.Eq Domain.ACTIVE_INVOICE,
          Se.Is BeamI.paymentMode $ Se.In allowedTypes
        ]
    ]
    (Se.Desc BeamI.createdAt)
    (Just 1)
    Nothing

updateInvoiceStatusByInvoiceId :: MonadFlow m => Domain.InvoiceStatus -> Id Domain.Invoice -> m ()
updateInvoiceStatusByInvoiceId invoiceStatus invoiceId = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamI.invoiceStatus invoiceStatus,
      Se.Set BeamI.updatedAt now
    ]
    [Se.Is BeamI.id (Se.Eq $ getId invoiceId)]

updateInvoiceStatusByDriverFeeIds :: MonadFlow m => Domain.InvoiceStatus -> [Id DriverFee] -> m ()
updateInvoiceStatusByDriverFeeIds status driverFeeIds = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamI.invoiceStatus status,
      Se.Set BeamI.updatedAt now
    ]
    [Se.Is BeamI.driverFeeId $ Se.In (getId <$> driverFeeIds)]

updateStatusAndTypeByMbdriverFeeIdAndInvoiceId :: MonadFlow m => Id Domain.Invoice -> Maybe Domain.InvoiceStatus -> Maybe Domain.InvoicePaymentMode -> Maybe (Id DriverFee) -> m ()
updateStatusAndTypeByMbdriverFeeIdAndInvoiceId invoiceId status paymentMode driverFeeId = do
  now <- getCurrentTime
  updateWithKV
    ( [Se.Set BeamI.updatedAt now]
        <> [Se.Set BeamI.invoiceStatus (fromJust status) | isJust status]
        <> [Se.Set BeamI.paymentMode (fromJust paymentMode) | isJust paymentMode]
    )
    ([Se.Is BeamI.driverFeeId $ Se.Eq (getId (fromJust driverFeeId)) | isJust driverFeeId] <> [Se.Is BeamI.id $ Se.Eq invoiceId.getId])

updatePendingToFailed :: MonadFlow m => NominalDiffTime -> m ()
updatePendingToFailed seconds = do
  endTime <- getCurrentTime
  let startTime = addUTCTime (-1 * seconds) endTime
  updateWithKV
    [Se.Set BeamI.invoiceStatus Domain.INACTIVE]
    [ Se.And
        [ Se.Is BeamI.invoiceStatus $ Se.Eq Domain.ACTIVE_INVOICE,
          Se.Is BeamI.createdAt $ Se.LessThan startTime
        ]
    ]

inActivateAllAutopayActiveInvoices :: MonadFlow m => Id Person -> m ()
inActivateAllAutopayActiveInvoices driverId = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamI.invoiceStatus Domain.INACTIVE,
      Se.Set BeamI.updatedAt now
    ]
    [ Se.And
        [ Se.Is BeamI.driverId $ Se.Eq (getId driverId),
          Se.Is BeamI.invoiceStatus $ Se.Eq Domain.ACTIVE_INVOICE,
          Se.Is BeamI.paymentMode $ Se.Eq Domain.AUTOPAY_INVOICE
        ]
    ]

updateBankErrorsByInvoiceId :: MonadFlow m => Maybe Text -> Maybe Text -> Id Domain.Invoice -> m ()
updateBankErrorsByInvoiceId bankError errorCode invoiceId = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamI.bankErrorMessage bankError,
      Se.Set BeamI.bankErrorCode errorCode,
      Se.Set BeamI.bankErrorUpdatedAt (Just now)
    ]
    [Se.Is BeamI.id (Se.Eq $ getId invoiceId)]

updateLastCheckedOn :: MonadFlow m => [Id Domain.Invoice] -> m ()
updateLastCheckedOn invoiceIds = do
  now <- getCurrentTime
  let lastCheckedAt = UTCTime (utctDay now) (secondsToDiffTime 0)
  updateWithKV
    [ Se.Set BeamI.lastStatusCheckedAt (Just lastCheckedAt),
      Se.Set BeamI.updatedAt now
    ]
    [Se.Is BeamI.id (Se.In $ getId <$> invoiceIds)]

instance FromTType' BeamI.Invoice Domain.Invoice where
  fromTType' BeamI.InvoiceT {..} = do
    pure $
      Just
        Invoice
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
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' BeamI.Invoice Domain.Invoice where
  toTType' Invoice {..} = do
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
        BeamI.createdAt = createdAt,
        BeamI.updatedAt = updatedAt
      }
