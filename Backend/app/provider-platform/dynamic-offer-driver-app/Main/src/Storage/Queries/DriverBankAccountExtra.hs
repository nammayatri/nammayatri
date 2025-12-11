module Storage.Queries.DriverBankAccountExtra where

import qualified Data.HashMap.Strict as HashMap
import Data.List (nub)
import Domain.Types.DriverBankAccount
import qualified Domain.Types.MerchantPaymentMethod as DMPM
import Domain.Types.Person
import qualified Domain.Types.Person as DP
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import Sequelize as Se
import qualified Storage.Beam.DriverBankAccount as Beam
import qualified Storage.Queries.FleetDriverAssociation as QFOA
import Storage.Queries.OrphanInstances.DriverBankAccount ()

-- Extra code goes here --
getDriverBankAccounts :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Kernel.Types.Id.Id Domain.Types.Person.Person] -> m [Domain.Types.DriverBankAccount.DriverBankAccount])
getDriverBankAccounts driverIds = do findAllWithKV [Se.And [Se.Is Beam.driverId $ Se.In (Kernel.Types.Id.getId <$> driverIds)]]

getDriverOrFleetBankAccounts ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Maybe DMPM.PaymentMode ->
  [Kernel.Types.Id.Id Domain.Types.Person.Person] ->
  m [(Id DP.Person, Domain.Types.DriverBankAccount.DriverBankAccount)]
getDriverOrFleetBankAccounts mbPaymentMode driverIds = do
  fleetDriverAssociations <- QFOA.findAllByDriverIds driverIds
  let fleetOwnerIds = nub $ fleetDriverAssociations <&> (Id @DP.Person . (.fleetOwnerId))
      driverIdsWithFleet = nub $ fleetDriverAssociations <&> (.driverId)
      driversIdsWithoutFleet = filter (`notElem` driverIdsWithFleet) driverIds
      personIds = fleetOwnerIds <> driversIdsWithoutFleet
      paymentMode = fromMaybe DMPM.LIVE mbPaymentMode
  personBankAccounts <- getDriverBankAccounts personIds

  let personBankAccountHashMap = HashMap.fromList $ (\pba -> (pba.driverId, pba)) <$> personBankAccounts
      fleetDriverAssociationHashMap = HashMap.fromList $ (\fda -> (fda.driverId, fda)) <$> fleetDriverAssociations
  let personBankAccountsMbList =
        driverIds <&> \driverId -> do
          personBankAccount <-
            if driverId `elem` driverIdsWithFleet
              then do
                fleetDriverAssociation <- HashMap.lookup driverId fleetDriverAssociationHashMap
                HashMap.lookup (Id @DP.Person fleetDriverAssociation.fleetOwnerId) personBankAccountHashMap -- fleet bank account
              else do
                HashMap.lookup driverId personBankAccountHashMap -- driver bank account
          let paymentMode' = fromMaybe DMPM.LIVE personBankAccount.paymentMode
          guard (paymentMode == paymentMode')
          pure (driverId, personBankAccount)
  pure $ catMaybes personBankAccountsMbList
