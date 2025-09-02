module Domain.Action.UI.Insurance (getInsurance) where

import qualified API.Types.UI.Insurance
import qualified Domain.Types.Insurance
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.RideStatus as DRide
import qualified Environment
import EulerHS.Prelude hiding (elem, id)
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.Insurance as SI
import qualified Storage.Queries.Insurance as QInsurance
import qualified Storage.Queries.Ride as QRide

getInsurance ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Insurance.Insurance ->
    Environment.Flow API.Types.UI.Insurance.InsuranceAPIEntity
  )
getInsurance _ insuranceId = do
  mbInsurance <- QInsurance.findById insuranceId
  case mbInsurance of
    Just insurance -> do
      return $
        API.Types.UI.Insurance.InsuranceAPIEntity
          { certificateUrl = insurance.certificateUrl,
            plan = Just insurance.plan,
            policyId = Just insurance.policyId,
            policyNumber = Just insurance.policyNumber,
            message = "Insurance generated!"
          }
    Nothing -> do
      ride <- QRide.findById (Kernel.Types.Id.Id insuranceId.getId) >>= fromMaybeM (InternalError "Ride not found")
      unless (ride.isInsured) $ throwError $ InvalidRequest "This booking is not insured!"
      unless (ride.status `elem` [DRide.INPROGRESS, DRide.COMPLETED]) $ throwError $ InvalidRequest "Insurance will be generated after ride is started!"
      fork "Try creating insurance again" $ SI.createInsurance ride
      return $
        API.Types.UI.Insurance.InsuranceAPIEntity
          { certificateUrl = Nothing,
            plan = Nothing,
            policyId = Nothing,
            policyNumber = Nothing,
            message = "Insurance not generated yet, please try again in sometime"
          }
