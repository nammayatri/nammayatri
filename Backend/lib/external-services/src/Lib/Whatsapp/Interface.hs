{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.Whatsapp.Interface
  ( module Reexport,
    whatsAppOptApi,
    whatsAppOtpApi,
  )
where

import EulerHS.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common
import Lib.Encryption
import Lib.Whatsapp.GupShup.Config as Reexport
import qualified Lib.Whatsapp.Interface.GupShup as GupShup
import Lib.Whatsapp.Interface.Types as Reexport
import Lib.Whatsapp.Types as Reexport

whatsAppOptApi :: (EncFlow m r, EsqDBFlow m r, CoreMetrics m) => WhatsappHandler m -> OptApiReq -> m OptApiResp
whatsAppOptApi WhatsappHandler {..} req = do
  prividersPriorityList <- getProvidersPriorityList
  when (null prividersPriorityList) $ throwError $ InternalError "No whatsapp serive provider configured"
  callWithFallback prividersPriorityList
  where
    callWithFallback [] = throwError $ InternalError "Not able to opt whatsapp with all the configured providers"
    callWithFallback (preferredProvider : restProviders) = do
      whatsappConfig <- getProviderConfig preferredProvider
      result <- try @_ @SomeException $ whatsAppOptApi' whatsappConfig req
      case result of
        Left _ -> callWithFallback restProviders
        Right res -> pure res

whatsAppOptApi' ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  WhatsappServiceConfig ->
  OptApiReq ->
  m OptApiResp
whatsAppOptApi' serviceConfig req = case serviceConfig of
  GupShupConfig cfg -> GupShup.whatsAppOptApi cfg req

whatsAppOtpApi :: (EncFlow m r, EsqDBFlow m r, CoreMetrics m) => WhatsappHandler m -> SendOtpApiReq -> m SendOtpApiResp
whatsAppOtpApi WhatsappHandler {..} req = do
  prividersPriorityList <- getProvidersPriorityList
  when (null prividersPriorityList) $ throwError $ InternalError "No whatsapp serive provider configured"
  callWithFallback prividersPriorityList
  where
    callWithFallback [] = throwError $ InternalError "Not able to opt whatsapp with all the configured providers"
    callWithFallback (preferredProvider : restProviders) = do
      whatsappConfig <- getProviderConfig preferredProvider
      result <- try @_ @SomeException $ whatsAppOtpApi' whatsappConfig req
      case result of
        Left _ -> callWithFallback restProviders
        Right res -> pure res

whatsAppOtpApi' ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  WhatsappServiceConfig ->
  SendOtpApiReq ->
  m SendOtpApiResp
whatsAppOtpApi' serviceConfig req = case serviceConfig of
  GupShupConfig cfg -> GupShup.whatsAppOTPApi cfg req
