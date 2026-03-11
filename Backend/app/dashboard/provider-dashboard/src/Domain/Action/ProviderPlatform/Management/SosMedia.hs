{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.ProviderPlatform.Management.SosMedia (getSosMediaSosMedia) where

import qualified API.Client.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.SosMedia
import qualified Dashboard.Common
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

getSosMediaSosMedia :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.Flow [API.Types.ProviderPlatform.Management.SosMedia.GetSosMediaResponse])
getSosMediaSosMedia merchantShortId opCity apiTokenInfo personId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.sosMediaDSL.getSosMediaSosMedia) personId
