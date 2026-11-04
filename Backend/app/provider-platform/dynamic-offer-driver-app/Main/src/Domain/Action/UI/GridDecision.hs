module Domain.Action.UI.GridDecision (postDriverGridVehiclePhotoDecision) where

import qualified API.Types.UI.GridDecision as API
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import qualified SharedLogic.VehiclePhotoVerification as VPV

-- | Proxies a driver's vehicle photo to Juspay Grid. Only the RC number and the photo come from the
-- client; the questions are fixed in 'VPV.verifyVehiclePhoto'.
postDriverGridVehiclePhotoDecision ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.VehiclePhotoDecisionReq ->
    Environment.Flow API.VehiclePhotoDecisionResp
  )
postDriverGridVehiclePhotoDecision (_, _, merchantOpCityId) req = do
  mbCheck <- VPV.verifyVehiclePhoto merchantOpCityId req.rcNumber req.imageBase64
  pure $ case mbCheck of
    Nothing ->
      API.VehiclePhotoDecisionResp
        { gridEnabled = False,
          acceptable = Nothing,
          vehicleType = Nothing,
          plateVisible = Nothing,
          plateMatchesRc = Nothing,
          isRealPhoto = Nothing,
          photoQuality = Nothing,
          model = Nothing
        }
    Just check ->
      API.VehiclePhotoDecisionResp
        { gridEnabled = True,
          acceptable = Just (VPV.isVehiclePhotoAcceptable check),
          vehicleType = check.vehicleType,
          plateVisible = check.plateVisible,
          plateMatchesRc = check.plateMatchesRc,
          isRealPhoto = check.isRealPhoto,
          photoQuality = check.photoQuality,
          model = check.model
        }
