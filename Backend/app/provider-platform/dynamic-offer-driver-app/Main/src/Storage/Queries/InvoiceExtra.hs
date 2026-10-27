module Storage.Queries.InvoiceExtra where

import Data.List (nubBy)
import Data.Time (UTCTime (UTCTime, utctDay), secondsToDiffTime)
import qualified Domain.Types.DriverFee as DF
import qualified Domain.Types.Invoice as Domain
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.Person (Person)
import Domain.Types.Plan (ServiceNames)
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Invoice as BeamI
import Storage.Queries.OrphanInstances.Invoice ()

-- Extra code goes here --

-- | All rows (one per driver fee) of an invoice.
-- The KV primary key of Invoice is ['id'] while the DB primary key is (id, driver_fee_id),
-- so every row of one invoice maps to the same KV key: KV holds a single row per invoice id and
-- findAllWithKV drops the DB rows sharing that key, returning just one fee. Merge the KV read
-- (latest, possibly not yet drained) with a DB read, de-duplicated on (id, driverFeeId).
findAllRowsByInvoiceId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Domain.Invoice -> m [Domain.Invoice]
findAllRowsByInvoiceId (Id invoiceId) = do
  kvRows <- findAllWithKV [Se.Is BeamI.id $ Se.Eq invoiceId]
  dbRows <- findAllWithDb [Se.Is BeamI.id $ Se.Eq invoiceId]
  pure $ nubBy (\a b -> a.driverFeeId == b.driverFeeId) (kvRows <> dbRows)

checkIfAutoPayInvoiceById :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Domain.Invoice -> m Bool
checkIfAutoPayInvoiceById (Id invoiceId) = do
  invoices <-
    findAllWithKV
      [ Se.And
          [ Se.Is BeamI.id $ Se.Eq invoiceId,
            Se.Is BeamI.paymentMode $ Se.Eq Domain.AUTOPAY_INVOICE
          ]
      ]
  return $ not (null invoices)

findValidByDriverFeeId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DF.DriverFee -> m [Domain.Invoice]
findValidByDriverFeeId (Id driverFeeId) =
  findAllWithKV
    [ Se.And
        [ Se.Is BeamI.driverFeeId $ Se.Eq driverFeeId,
          Se.Is BeamI.invoiceStatus $ Se.Not $ Se.In [Domain.INACTIVE, Domain.EXPIRED]
        ]
    ]

findAllInvoicesByDriverIdWithLimitAndOffset ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
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

findLatestByDriverFeeId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DF.DriverFee -> m (Maybe Domain.Invoice)
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

findActiveManualOrMandateSetupInvoiceByFeeId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DF.DriverFee -> m [Domain.Invoice]
findActiveManualOrMandateSetupInvoiceByFeeId (Id driverFeeId) =
  findAllWithKV
    [ Se.And
        [ Se.Is BeamI.driverFeeId $ Se.Eq driverFeeId,
          Se.Is BeamI.invoiceStatus $ Se.Eq Domain.ACTIVE_INVOICE,
          Se.Is BeamI.paymentMode $ Se.In [Domain.MANUAL_INVOICE, Domain.MANDATE_SETUP_INVOICE]
        ]
    ]

findByDriverFeeIds :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Id DF.DriverFee] -> m [Domain.Invoice]
findByDriverFeeIds driverFeeIds =
  findAllWithKV
    [Se.Is BeamI.driverFeeId $ Se.In (getId <$> driverFeeIds)]

findActiveByDriverFeeIds :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Id DF.DriverFee] -> m [Domain.Invoice]
findActiveByDriverFeeIds driverFeeIds =
  findAllWithKV
    [ Se.And
        [ Se.Is BeamI.driverFeeId $ Se.In (getId <$> driverFeeIds),
          Se.Is BeamI.invoiceStatus $ Se.Eq Domain.ACTIVE_INVOICE
        ]
    ]

findAllByStatusWithLimit ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
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

-- | Manual / mandate-setup invoices marked FAILED in the last 24h and not yet re-checked today.
-- A failed attempt can be followed by a successful one on the same order; once the invoice is
-- FAILED it is no longer ACTIVE, so without this the order is never re-synced and stays failed.
findRecentFailedWithLimit ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  Int ->
  m [Domain.Invoice]
findRecentFailedWithLimit merchantOperatingCityId limit = do
  endTime <- getCurrentTime
  let startTime = addUTCTime (- (1 * 3600 * 24)) endTime
  let lastCheckedAt = UTCTime (utctDay endTime) (secondsToDiffTime 0)
  findAllWithOptionsKV'
    [ Se.And
        [ Se.Is BeamI.invoiceStatus $ Se.Eq Domain.FAILED,
          Se.Is BeamI.paymentMode $ Se.In [Domain.MANUAL_INVOICE, Domain.MANDATE_SETUP_INVOICE],
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
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  NominalDiffTime ->
  Id DMOC.MerchantOperatingCity ->
  Int ->
  m [Domain.Invoice]
findAllAutoPayInvoicesActiveOlderThanProvidedDuration timeDiff merchantOperatingCityId limit = do
  endTime <- getCurrentTime
  let startTime = addUTCTime (-1 * timeDiff) endTime
  findAllWithOptionsKV'
    [ Se.And
        [ Se.Is BeamI.invoiceStatus $ Se.Eq Domain.ACTIVE_INVOICE,
          Se.Is BeamI.merchantOperatingCityId $ Se.Eq (Just $ getId merchantOperatingCityId),
          Se.Is BeamI.createdAt $ Se.LessThan startTime
        ]
    ]
    (Just limit)
    Nothing

findLatestAutopayActiveByDriverFeeId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DF.DriverFee -> m [Domain.Invoice]
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

findLatestNonAutopayActiveByDriverId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> ServiceNames -> m [Domain.Invoice]
findLatestNonAutopayActiveByDriverId driverId serviceName = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is BeamI.driverId $ Se.Eq (getId driverId),
          Se.Is BeamI.paymentMode $ Se.Not $ Se.In [Domain.AUTOPAY_INVOICE, Domain.PAYOUT_REGISTRATION_INVOICE, Domain.ONE_TIME_SECURITY_INVOICE],
          Se.Is BeamI.serviceName $ Se.Eq (Just serviceName)
        ]
    ]
    (Se.Desc BeamI.createdAt)
    (Just 1)
    Nothing

reactivateAutopayInvoiceByDriverFeeId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DF.DriverFee -> m ()
reactivateAutopayInvoiceByDriverFeeId driverFeeId = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamI.invoiceStatus Domain.ACTIVE_INVOICE,
      Se.Set BeamI.updatedAt now
    ]
    [ Se.And
        [ Se.Is BeamI.driverFeeId $ Se.Eq (getId driverFeeId),
          Se.Is BeamI.paymentMode $ Se.Eq Domain.AUTOPAY_INVOICE,
          Se.Is BeamI.invoiceStatus $ Se.In [Domain.ACTIVE_INVOICE, Domain.INACTIVE]
        ]
    ]

retireAutopayInvoicesByDriverFeeIds :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Id DF.DriverFee] -> m ()
retireAutopayInvoicesByDriverFeeIds driverFeeIds = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamI.invoiceStatus Domain.EXPIRED,
      Se.Set BeamI.updatedAt now
    ]
    [ Se.And
        [ Se.Is BeamI.driverFeeId $ Se.In (getId <$> driverFeeIds),
          Se.Is BeamI.paymentMode $ Se.Eq Domain.AUTOPAY_INVOICE,
          Se.Is BeamI.invoiceStatus $ Se.In [Domain.ACTIVE_INVOICE, Domain.INACTIVE]
        ]
    ]

-- | Set the status of specific rows (one per driver fee) of an invoice, e.g. after a partial
-- payment only the rows of the fees that payment covered become SUCCESS.
updateInvoiceStatusByInvoiceIdAndDriverFeeIds :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Domain.InvoiceStatus -> Id Domain.Invoice -> [Id DF.DriverFee] -> m ()
updateInvoiceStatusByInvoiceIdAndDriverFeeIds invoiceStatus invoiceId driverFeeIds =
  unless (null driverFeeIds) $ do
    now <- getCurrentTime
    updateWithKV
      [ Se.Set BeamI.invoiceStatus invoiceStatus,
        Se.Set BeamI.updatedAt now
      ]
      [ Se.And
          [ Se.Is BeamI.id (Se.Eq $ getId invoiceId),
            Se.Is BeamI.driverFeeId (Se.In $ getId <$> driverFeeIds)
          ]
      ]

updateInvoiceStatusByInvoiceId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Domain.InvoiceStatus -> Id Domain.Invoice -> m ()
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

updateInvoiceStatusByDriverFeeIdsAndMbPaymentMode :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Domain.InvoiceStatus -> [Id DF.DriverFee] -> Maybe Domain.InvoicePaymentMode -> m ()
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

findAllActiveByDriverFeeIds :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Id DF.DriverFee] -> m [Domain.Invoice]
findAllActiveByDriverFeeIds driverFeeIds =
  findAllWithKV
    [ Se.And
        [ Se.Is BeamI.driverFeeId $ Se.In (getId <$> driverFeeIds),
          Se.Is BeamI.invoiceStatus $ Se.Eq Domain.ACTIVE_INVOICE
        ]
    ]

updateActiveInvoiceStatusByDriverFeeIdsAndPaymentMode :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Domain.InvoiceStatus -> [Id DF.DriverFee] -> Domain.InvoicePaymentMode -> m ()
updateActiveInvoiceStatusByDriverFeeIdsAndPaymentMode status driverFeeIds paymentMode = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamI.invoiceStatus status,
      Se.Set BeamI.updatedAt now
    ]
    [ Se.And
        [ Se.Is BeamI.driverFeeId $ Se.In (getId <$> driverFeeIds),
          Se.Is BeamI.paymentMode $ Se.Eq paymentMode,
          Se.Is BeamI.invoiceStatus $ Se.Eq Domain.ACTIVE_INVOICE
        ]
    ]

updateStatusAndTypeByMbdriverFeeIdAndInvoiceId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Domain.Invoice -> Maybe Domain.InvoiceStatus -> Maybe Domain.InvoicePaymentMode -> Maybe (Id DF.DriverFee) -> m ()
updateStatusAndTypeByMbdriverFeeIdAndInvoiceId invoiceId status paymentMode driverFeeId = do
  now <- getCurrentTime
  updateWithKV
    ( [Se.Set BeamI.updatedAt now]
        <> [Se.Set BeamI.invoiceStatus (fromJust status) | isJust status]
        <> [Se.Set BeamI.paymentMode (fromJust paymentMode) | isJust paymentMode]
    )
    ([Se.Is BeamI.driverFeeId $ Se.Eq (getId (fromJust driverFeeId)) | isJust driverFeeId] <> [Se.Is BeamI.id $ Se.Eq invoiceId.getId])

updatePendingToFailed ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  [Id Domain.Invoice] ->
  Id DMOC.MerchantOperatingCity ->
  m ()
updatePendingToFailed invoiceIds merchantOperatingCityId =
  updateWithKV
    [Se.Set BeamI.invoiceStatus Domain.INACTIVE]
    [ Se.And
        [ Se.Is BeamI.id $ Se.In (getId <$> invoiceIds),
          Se.Is BeamI.invoiceStatus $ Se.Eq Domain.ACTIVE_INVOICE,
          Se.Is BeamI.merchantOperatingCityId $ Se.Eq (Just merchantOperatingCityId.getId)
        ]
    ]

updateLastCheckedOn :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Id Domain.Invoice] -> m ()
updateLastCheckedOn invoiceIds = do
  now <- getCurrentTime
  let lastCheckedAt = UTCTime (utctDay now) (secondsToDiffTime 0)
  updateWithKV
    [ Se.Set BeamI.lastStatusCheckedAt (Just lastCheckedAt),
      Se.Set BeamI.updatedAt now
    ]
    [Se.Is BeamI.id (Se.In $ getId <$> invoiceIds)]

inActivateAllAutopayActiveInvoices :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> ServiceNames -> m ()
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
