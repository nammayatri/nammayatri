{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.Cac where

import qualified Data.Aeson
import qualified Data.Aeson.KeyMap
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified Kernel.Beam.Types as KBT
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Servant
import SharedLogic.Cac
import Tools.Auth

getDriverGetUiConfigs :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant, Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity) -> Kernel.Prelude.Int -> Environment.Flow Data.Aeson.Object
getDriverGetUiConfigs (_, _, merchantOpCityId) toss = do
  systemConfigs <- L.getOption KBT.Tables
  let useCACConfig = maybe False (.useCACForFrontend) systemConfigs
  if useCACConfig
    then getFrontendConfigs merchantOpCityId (Just toss) <&> fromMaybe Data.Aeson.KeyMap.empty
    else return Data.Aeson.KeyMap.empty
