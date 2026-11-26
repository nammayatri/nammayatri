{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.AppAttestation
  ( module Reexport,
    AttestationGateFlow,
    driverAppService,
    runGate,
    findAttestationConfig,
    userKindOf,
  )
where

import AppAttestation.Canonical as Reexport (CanonicalRequest (..))
import AppAttestation.Domain.Types.AppAttestationConfig (AppAttestationConfig)
import qualified AppAttestation.Domain.Types.AppAttestationConfig as Fields
import qualified AppAttestation.Domain.Types.Common as AppAttestation
import AppAttestation.Gate as Reexport (GateRequest (..), GateVerdict (..), SilentDrop (..), findSilentDrop, isSilentlyDropped, markSilentlyDropped, useSilentDropAttempt)
import qualified AppAttestation.Gate as Gate
import AppAttestation.Types as Reexport (AttestationHeaders (..), Endpoint (..), UserKind (..))
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DMSC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.TransporterConfig as Fields
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import Storage.Beam.AppAttestation ()
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.MerchantServiceConfig (MerchantServiceConfigDimensions (..))
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import Tools.Error

type AttestationGateFlow m r = Gate.GateFlow m r

driverAppService :: Text
driverAppService = "dynamic-offer-driver-app"

runGate :: AttestationGateFlow m r => Id DMOC.MerchantOperatingCity -> GateRequest -> m GateVerdict
runGate merchantOperatingCityId = Gate.runGate (loadContext merchantOperatingCityId)

loadContext :: AttestationGateFlow m r => Id DMOC.MerchantOperatingCity -> m (Maybe (Gate.AttestationContext m))
loadContext merchantOperatingCityId =
  findAttestationConfig merchantOperatingCityId >>= \case
    Just config | config.mode /= AppAttestation.OFF -> do
      merchantOperatingCity <- CQMOC.findById merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound merchantOperatingCityId.getId)
      pure $
        Just
          Gate.AttestationContext
            { merchantId = cast merchantOperatingCity.merchantId,
              merchantOperatingCityId = cast merchantOperatingCityId,
              config,
              loadPlayIntegrityConfig =
                findServiceConfig (DMSC.AppAttestationService AppAttestation.PlayIntegrity) <&> \case
                  Just (DMSC.AppAttestationServiceConfig playIntegrityConfig) -> Just playIntegrityConfig
                  _ -> Nothing,
              loadCaptchaConfig =
                findServiceConfig (DMSC.CaptchaService AppAttestation.SiteVerify) <&> \case
                  Just (DMSC.CaptchaServiceConfig captchaConfig) -> Just captchaConfig
                  _ -> Nothing
            }
    _ -> pure Nothing
  where
    findServiceConfig serviceName =
      fmap (.serviceConfig)
        <$> getOneConfig
          MerchantServiceConfigDimensions
            { merchantOperatingCityId = merchantOperatingCityId.getId,
              merchantId = Nothing,
              serviceName = Just serviceName
            }
          Nothing

findAttestationConfig :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> m (Maybe AppAttestationConfig)
findAttestationConfig merchantOperatingCityId =
  (>>= (.appAttestationConfig)) <$> getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOperatingCityId.getId}) Nothing

userKindOf :: DP.Person -> UserKind
userKindOf person = if person.isNew then NewUser else ExistingUser
