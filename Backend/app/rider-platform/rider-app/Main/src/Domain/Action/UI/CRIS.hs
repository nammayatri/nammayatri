{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.CRIS (postCrisGetSDKData) where

import qualified API.Types.UI.CRIS
import qualified BecknV2.OnDemand.Enums
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import ExternalBPP.ExternalAPI.Subway.CRIS.SDKData as GetSDKData
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM, throwError)
import qualified Storage.Queries.IntegratedBPPConfig as QIntegratedBPPConfig
import qualified Storage.Queries.Person as QP

postCrisGetSDKData ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.CRIS.GetSDKDataRequest ->
    Environment.Flow API.Types.UI.CRIS.GetSDKDataResponse
  )
postCrisGetSDKData (mbPersonId, _) request = do
  case mbPersonId of
    Just personId -> do
      person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
      integratedBPPConfig <- QIntegratedBPPConfig.findByDomainAndCityAndVehicleCategory "FRFS" person.merchantOperatingCityId BecknV2.OnDemand.Enums.SUBWAY DIBC.MULTIMODAL >>= fromMaybeM (InternalError "No integrated bpp config found")
      case integratedBPPConfig.providerConfig of
        DIBC.CRIS config' -> do
          resp <- try @_ @SomeException $ GetSDKData.getSDKData config' request
          case resp of
            Left _ -> throwError $ InternalError "Error in get-sdk-data!"
            Right sdkData -> do
              case sdkData.respCode of
                0 -> return sdkData
                _ -> throwError $ InternalError "non-zero respCode from get-sdk-data!"
        _ -> throwError $ InternalError "Unimplemented!"
    Nothing -> throwError $ InternalError "Person not found"
