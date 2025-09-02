module Domain.Action.UI.Insurance (getInsurance) where

import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions (runInReplica)
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.CallBAPInternal
import qualified Storage.Queries.Ride as QRide

getInsurance ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Text ->
    Environment.Flow SharedLogic.CallBAPInternal.InsuranceAPIEntity
  )
getInsurance _ rideId = do
  ride <- runInReplica $ QRide.findById (Kernel.Types.Id.Id rideId) >>= fromMaybeM (RideDoesNotExist rideId)
  unless (ride.isInsured) $ throwError $ InvalidRequest "This ride is not insured!"
  appBackendBapInternal <- asks (.appBackendBapInternal)
  SharedLogic.CallBAPInternal.getInsuranceInfo appBackendBapInternal.apiKey appBackendBapInternal.url rideId
