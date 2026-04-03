{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.DriverBankAccount (module Storage.Queries.DriverBankAccount, module ReExport) where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import Storage.Queries.DriverBankAccountExtra as ReExport
import qualified Domain.Types.DriverBankAccount
import qualified Storage.Beam.DriverBankAccount as Beam
import qualified Kernel.Prelude
import qualified Servant.Client.Core
import qualified Kernel.Types.Id
import qualified Domain.Types.Person
import qualified Sequelize as Se



create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverBankAccount.DriverBankAccount -> m ())
create = createWithKV
createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.DriverBankAccount.DriverBankAccount] -> m ())
createMany = traverse_ create
deleteById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
deleteById driverId = do deleteWithKV [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]
updateAccountLink :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                     (Kernel.Prelude.Maybe Servant.Client.Core.BaseUrl -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateAccountLink currentAccountLink currentAccountLinkExpiry driverId = do {_now <- getCurrentTime;
                                                                             updateOneWithKV [Se.Set Beam.currentAccountLink (Kernel.Prelude.fmap showBaseUrl currentAccountLink),
                                                                                              Se.Set Beam.currentAccountLinkExpiry currentAccountLinkExpiry,
                                                                                              Se.Set Beam.updatedAt _now] [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]}
updateAccountStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Bool -> Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateAccountStatus chargesEnabled detailsSubmitted driverId = do {_now <- getCurrentTime;
                                                                   updateOneWithKV [Se.Set Beam.chargesEnabled chargesEnabled, Se.Set Beam.detailsSubmitted detailsSubmitted, Se.Set Beam.updatedAt _now] [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]}
findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.DriverBankAccount.DriverBankAccount))
findByPrimaryKey driverId = do findOneWithKV [Se.And [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]]
updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverBankAccount.DriverBankAccount -> m ())
updateByPrimaryKey (Domain.Types.DriverBankAccount.DriverBankAccount {..}) = do {_now <- getCurrentTime;
                                                                                 updateWithKV [Se.Set Beam.accountId accountId,
                                                                                               Se.Set Beam.chargesEnabled chargesEnabled,
                                                                                               Se.Set Beam.currentAccountLink (Kernel.Prelude.fmap showBaseUrl currentAccountLink),
                                                                                               Se.Set Beam.currentAccountLinkExpiry currentAccountLinkExpiry,
                                                                                               Se.Set Beam.detailsSubmitted detailsSubmitted,
                                                                                               Se.Set Beam.ifscCode ifscCode,
                                                                                               Se.Set Beam.nameAtBank nameAtBank,
                                                                                               Se.Set Beam.paymentMode paymentMode,
                                                                                               Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
                                                                                               Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
                                                                                               Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]]}



