{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.Insurance (getInsurance) where

import qualified API.Types.UI.Insurance
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.Ride as DRide
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions (runInReplica)
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant hiding (throwError)
import qualified SharedLogic.CallBAPInternal
import qualified Storage.Queries.Ride as QRide
import Tools.Auth
import Tools.Error

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
  unless (ride.status == DRide.INPROGRESS) $ throwError $ InvalidRequest "Insurance will be generated after ride is started!"
  appBackendBapInternal <- asks (.appBackendBapInternal)
  SharedLogic.CallBAPInternal.getInsuranceInfo appBackendBapInternal.apiKey appBackendBapInternal.url rideId
