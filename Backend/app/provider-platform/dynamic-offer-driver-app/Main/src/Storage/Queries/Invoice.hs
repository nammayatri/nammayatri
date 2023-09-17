{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Invoice where

import Domain.Types.DriverFee (DriverFee)
import Domain.Types.Invoice as Domain
import Kernel.Beam.Functions (FromTType' (fromTType'), ToTType' (toTType'), createWithKV, findAllWithKV, findOneWithKV, updateWithKV)
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import qualified Sequelize as Se
import Storage.Beam.Invoice as BeamI hiding (Id)

create :: MonadFlow m => Domain.Invoice -> m ()
create = createWithKV

findById :: MonadFlow m => Id Domain.Invoice -> m [Domain.Invoice]
findById (Id invoiceId) = findAllWithKV [Se.Is BeamI.id $ Se.Eq invoiceId]

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

findValidByInvoiceIdWithWindow :: MonadFlow m => Id Domain.Invoice -> Domain.InvoiceStatus -> UTCTime -> m [Domain.Invoice]
findValidByInvoiceIdWithWindow (Id invoiceId) status from =
  findAllWithKV
    [ Se.And
        [ Se.Is BeamI.id $ Se.Eq invoiceId,
          Se.Is BeamI.invoiceStatus $ Se.Eq status,
          Se.Is BeamI.createdAt $ Se.GreaterThanOrEq from
        ]
    ]

findAllByInvoiceId :: MonadFlow m => Id Domain.Invoice -> m [Domain.Invoice]
findAllByInvoiceId (Id invoiceId) = findAllWithKV [Se.Is BeamI.id $ Se.Eq invoiceId]

findByIdWithPaymenModeAndStatus :: MonadFlow m => Id Domain.Invoice -> Domain.InvoicePaymentMode -> Domain.InvoiceStatus -> m (Maybe Domain.Invoice)
findByIdWithPaymenModeAndStatus invocieId paymentMode status =
  findOneWithKV
    [ Se.And
        [ Se.Is BeamI.id $ Se.Eq invocieId.getId,
          Se.Is BeamI.invoiceStatus $ Se.Eq status,
          Se.Is BeamI.paymentMode $ Se.Eq paymentMode
        ]
    ]

findByDriverFeeIdAndActiveStatus :: MonadFlow m => Id DriverFee -> m [Domain.Invoice]
findByDriverFeeIdAndActiveStatus (Id driverFeeId) =
  findAllWithKV
    [ Se.And
        [ Se.Is BeamI.driverFeeId $ Se.Eq driverFeeId,
          Se.Is BeamI.invoiceStatus $ Se.Eq Domain.ACTIVE_INVOICE
        ]
    ]

findByDriverFeeIds :: MonadFlow m => [Id DriverFee] -> m [Domain.Invoice]
findByDriverFeeIds driverFeeIds =
  findAllWithKV
    [Se.Is BeamI.driverFeeId $ Se.In (getId <$> driverFeeIds)]

findAllByDriverFeeIdAndStatus :: MonadFlow m => [Id DriverFee] -> Domain.InvoiceStatus -> [Domain.InvoicePaymentMode] -> m [Domain.Invoice]
findAllByDriverFeeIdAndStatus driverIds status paymentMode = findAllWithKV [Se.And [Se.Is BeamI.driverFeeId $ Se.In (driverIds <&> getId), Se.Is BeamI.invoiceStatus $ Se.Eq status, Se.Is BeamI.paymentMode $ Se.In paymentMode]]

-- findLatestActiveInvoiceForDriverFee :: MonadFlow m =>

updateInvoiceStatusByInvoiceId :: MonadFlow m => Domain.InvoiceStatus -> Id Domain.Invoice -> m ()
updateInvoiceStatusByInvoiceId invoiceStatus invoiceId = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamI.invoiceStatus invoiceStatus,
      Se.Set BeamI.updatedAt now
    ]
    [Se.Is BeamI.id (Se.Eq $ getId invoiceId)]

updateBankErrorsByInvoiceId :: MonadFlow m => Maybe Text -> Maybe Text -> Id Domain.Invoice -> m ()
updateBankErrorsByInvoiceId bankError errorCode invoiceId = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamI.bankErrorMessage bankError,
      Se.Set BeamI.bankErrorCode errorCode,
      Se.Set BeamI.bankErrorUpdatedAt (Just now)
    ]
    [Se.Is BeamI.id (Se.Eq $ getId invoiceId)]

instance FromTType' BeamI.Invoice Domain.Invoice where
  fromTType' BeamI.InvoiceT {..} = do
    pure $
      Just
        Invoice
          { id = Id id,
            invoiceShortId = invoiceShortId,
            driverFeeId = Id driverFeeId,
            invoiceStatus = invoiceStatus,
            paymentMode = paymentMode,
            maxMandateAmount = maxMandateAmount,
            bankErrorCode,
            bankErrorMessage,
            bankErrorUpdatedAt,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' BeamI.Invoice Domain.Invoice where
  toTType' Invoice {..} = do
    BeamI.InvoiceT
      { BeamI.id = id.getId,
        BeamI.invoiceShortId = invoiceShortId,
        BeamI.driverFeeId = getId driverFeeId,
        BeamI.paymentMode = paymentMode,
        BeamI.invoiceStatus = invoiceStatus,
        BeamI.maxMandateAmount = maxMandateAmount,
        BeamI.bankErrorCode = bankErrorCode,
        BeamI.bankErrorMessage = bankErrorMessage,
        BeamI.bankErrorUpdatedAt = bankErrorUpdatedAt,
        BeamI.createdAt = createdAt,
        BeamI.updatedAt = updatedAt
      }
