{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.RiderPlatform.Management.SosMedia
  ( getSosMediaSosMedia,
  )
where

import qualified API.Client.RiderPlatform.Management
import qualified API.Types.RiderPlatform.Management.SosMedia
import qualified Dashboard.Common
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified "lib-dashboard" SharedLogic.Transaction
import qualified Kernel.Types.Id
import Kernel.Utils.Common ()
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

getSosMediaSosMedia ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Kernel.Types.Id.Id Dashboard.Common.Customer ->
  Environment.Flow [API.Types.RiderPlatform.Management.SosMedia.GetSosMediaResponse]
getSosMediaSosMedia merchantShortId opCity apiTokenInfo customerId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  SharedLogic.Transaction.withGetTransactionStoring
    (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType)
    (Kernel.Prelude.Just APP_BACKEND)
    (Kernel.Prelude.Just apiTokenInfo)
    Kernel.Prelude.Nothing
    Kernel.Prelude.Nothing
    (API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.sosMediaDSL.getSosMediaSosMedia) customerId)
