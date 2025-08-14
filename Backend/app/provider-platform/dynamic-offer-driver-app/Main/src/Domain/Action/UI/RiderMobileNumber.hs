{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.RiderMobileNumber (getRiderMobileNumber) where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Servant
import qualified SharedLogic.CallBAPInternal
import Tools.Auth

getRiderMobileNumber ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Text ->
    Environment.Flow SharedLogic.CallBAPInternal.RiderMobileAPIEntity
  )
getRiderMobileNumber _ rideId = do
  appBackendBapInternal <- asks (.appBackendBapInternal)
  SharedLogic.CallBAPInternal.getRiderMobileNumber appBackendBapInternal.apiKey appBackendBapInternal.url rideId
