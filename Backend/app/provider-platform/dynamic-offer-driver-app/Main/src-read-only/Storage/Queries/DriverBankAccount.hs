{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DriverBankAccount where

import qualified Domain.Types.DriverBankAccount
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Servant.Client.Core
import qualified Storage.Beam.DriverBankAccount as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverBankAccount.DriverBankAccount -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.DriverBankAccount.DriverBankAccount] -> m ())
createMany = traverse_ create

updateAccountLink ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Servant.Client.Core.BaseUrl -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateAccountLink currentAccountLink currentAccountLinkExpiry driverId = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.currentAccountLink (Kernel.Prelude.fmap showBaseUrl currentAccountLink),
      Se.Set Beam.currentAccountLinkExpiry currentAccountLinkExpiry,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

updateAccountStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Bool -> Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateAccountStatus chargesEnabled detailsSubmitted driverId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.chargesEnabled chargesEnabled, Se.Set Beam.detailsSubmitted detailsSubmitted, Se.Set Beam.updatedAt _now] [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.DriverBankAccount.DriverBankAccount))
findByPrimaryKey driverId = do findOneWithKV [Se.And [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverBankAccount.DriverBankAccount -> m ())
updateByPrimaryKey (Domain.Types.DriverBankAccount.DriverBankAccount {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.accountId accountId,
      Se.Set Beam.chargesEnabled chargesEnabled,
      Se.Set Beam.currentAccountLink (Kernel.Prelude.fmap showBaseUrl currentAccountLink),
      Se.Set Beam.currentAccountLinkExpiry currentAccountLinkExpiry,
      Se.Set Beam.detailsSubmitted detailsSubmitted,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]]

instance FromTType' Beam.DriverBankAccount Domain.Types.DriverBankAccount.DriverBankAccount where
  fromTType' (Beam.DriverBankAccountT {..}) = do
    currentAccountLink' <- Kernel.Prelude.mapM parseBaseUrl currentAccountLink
    pure $
      Just
        Domain.Types.DriverBankAccount.DriverBankAccount
          { accountId = accountId,
            chargesEnabled = chargesEnabled,
            currentAccountLink = currentAccountLink',
            currentAccountLinkExpiry = currentAccountLinkExpiry,
            detailsSubmitted = detailsSubmitted,
            driverId = Kernel.Types.Id.Id driverId,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.DriverBankAccount Domain.Types.DriverBankAccount.DriverBankAccount where
  toTType' (Domain.Types.DriverBankAccount.DriverBankAccount {..}) = do
    Beam.DriverBankAccountT
      { Beam.accountId = accountId,
        Beam.chargesEnabled = chargesEnabled,
        Beam.currentAccountLink = Kernel.Prelude.fmap showBaseUrl currentAccountLink,
        Beam.currentAccountLinkExpiry = currentAccountLinkExpiry,
        Beam.detailsSubmitted = detailsSubmitted,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }

{-
	DSL Source Link: file://./../../../spec/Storage/DriverOnboarding.yaml
-}
