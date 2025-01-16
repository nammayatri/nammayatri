{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Action.ProviderPlatform.Management.Media (getMediaMediaImage) where

import qualified API.Client.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.Media
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified "shared-services" IssueManagement.Domain.Types.MediaFile
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

getMediaMediaImage :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id IssueManagement.Domain.Types.MediaFile.MediaFile -> Environment.Flow API.Types.ProviderPlatform.Management.Media.GetImageResponse)
getMediaMediaImage merchantShortId opCity apiTokenInfo imageId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.mediaDSL.getMediaMediaImage) imageId
