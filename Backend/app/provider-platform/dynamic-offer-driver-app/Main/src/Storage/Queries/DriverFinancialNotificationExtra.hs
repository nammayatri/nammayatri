{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DriverFinancialNotificationExtra where

import qualified Domain.Types.DriverFinancialNotification
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverFinancialNotification as Beam
import Storage.Queries.OrphanInstances.DriverFinancialNotification

findAllByDriverIdWithLimitOffset ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Domain.Types.Person.Person ->
  Maybe Domain.Types.DriverFinancialNotification.DriverNotificationCategory ->
  Maybe Bool ->
  Maybe Int ->
  Maybe Int ->
  m [Domain.Types.DriverFinancialNotification.DriverFinancialNotification]
findAllByDriverIdWithLimitOffset driverId mbCategory mbIsRead mbLimit mbOffset = do
  let limitVal = min 50 $ fromMaybe 20 mbLimit
      offsetVal = fromMaybe 0 mbOffset
  findAllWithOptionsKV
    [ Se.And $
        [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]
          <> maybe [] (\c -> [Se.Is Beam.category $ Se.Eq c]) mbCategory
          <> maybe [] (\r -> [Se.Is Beam.isRead $ Se.Eq r]) mbIsRead
    ]
    (Se.Desc Beam.createdAt)
    (Just limitVal)
    (Just offsetVal)

countUnreadByDriverId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Domain.Types.Person.Person ->
  m Int
countUnreadByDriverId driverId = do
  unread <- findAllWithKV
    [ Se.And
        [ Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId),
          Se.Is Beam.isRead $ Se.Eq False
        ]
    ]
  pure $ length unread
