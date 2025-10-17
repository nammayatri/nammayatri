{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.DriverToDriverDirectCalling (getDriverGetnumber) where

import API.Types.UI.DriverToDriverDirectCalling
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Domain.Types.Person as Person
import qualified Domain.Types.Person
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption (decrypt, encrypt, getDbHash)
import Kernel.External.Types
import Kernel.Prelude
import Kernel.Types.Id
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant hiding (throwError)
import qualified Storage.Queries.DriverRCAssociation as DAQuery
import Storage.Queries.Person as PSQuery
import qualified Storage.Queries.VehicleRegistrationCertificate as RCQuery
import Tools.Auth
import Tools.Error

getDriverGetnumber ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Text ->
    Environment.Flow API.Types.UI.DriverToDriverDirectCalling.DriverGetnumberResp
  )
getDriverGetnumber (_, mid, _) rcNo = do
  vehicleRC <- RCQuery.findLastVehicleRCWrapper rcNo >>= fromMaybeM (RCNotFound rcNo)
  rcActiveAssociation <- DAQuery.findActiveAssociationByRC vehicleRC.id True >>= fromMaybeM ActiveRCNotFound
  getDecryptedMobileNumberByDriverId rcActiveAssociation.driverId mid

getDecryptedMobileNumberByDriverId :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r) => Id Person.Person -> Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> m API.Types.UI.DriverToDriverDirectCalling.DriverGetnumberResp
getDecryptedMobileNumberByDriverId driverId mid = do
  driver <- PSQuery.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  if driver.merchantId == mid
    then case driver.mobileNumber of
      Just mobNum -> do
        decryptedNumber <- decrypt mobNum
        return $ API.Types.UI.DriverToDriverDirectCalling.DriverGetnumberResp {mobileNumber = decryptedNumber, name = driver.firstName}
      Nothing -> throwError $ InvalidRequest "Mobile Number not found."
    else throwError $ InvalidRequest "Merchant Id Did not match"
