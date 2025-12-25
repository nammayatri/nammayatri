module Domain.Action.UI.InsuranceInternal (getInsurance) where

import qualified API.Types.UI.Insurance as Insurance
import qualified Domain.Types.RideStatus as DRide
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.Insurance as SI
import qualified Storage.Queries.Insurance as QInsurance
import qualified Storage.Queries.Ride as QRide

getInsurance :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.Flow Insurance.InsuranceAPIEntity)
getInsurance bppRideId mbToken = do
  internalAPIKey <- asks (.internalAPIKey)
  unless (Just internalAPIKey == mbToken) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  ride <- QRide.findByBPPRideId (Kernel.Types.Id.Id bppRideId) >>= fromMaybeM (InternalError "Ride not found")
  unless (ride.isInsured) $ throwError $ InvalidRequest "This booking is not insured!"
  mbInsurance <- QInsurance.findById (Kernel.Types.Id.Id ride.id.getId)
  case mbInsurance of
    Just insurance -> do
      return $
        Insurance.InsuranceAPIEntity
          { certificateUrl = insurance.certificateUrl,
            plan = Just insurance.plan,
            policyId = Just insurance.policyId,
            policyNumber = Just insurance.policyNumber,
            message = "Insurance generated!"
          }
    Nothing -> do
      unless (ride.status `elem` [DRide.INPROGRESS, DRide.COMPLETED]) $ throwError $ InvalidRequest "Insurance will be generated after ride is started!"
      fork "Try creating insurance again" $ SI.createInsurance ride
      return $
        Insurance.InsuranceAPIEntity
          { certificateUrl = Nothing,
            plan = Nothing,
            policyId = Nothing,
            policyNumber = Nothing,
            message = "Insurance not generated yet, please try again in sometime"
          }
