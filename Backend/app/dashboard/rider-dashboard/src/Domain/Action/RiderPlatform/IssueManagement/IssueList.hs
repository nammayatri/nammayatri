module Domain.Action.RiderPlatform.IssueManagement.IssueList
  ( getIssueListV1,
    postIssueListTicketStatusCallBack,
  )
where

import qualified API.Client.RiderPlatform.IssueManagement
import qualified API.Types.RiderPlatform.IssueManagement.IssueList
import qualified Data.Aeson
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

getIssueListV1 ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Kernel.Prelude.Maybe Kernel.Prelude.UTCTime ->
  Kernel.Prelude.Maybe Kernel.Prelude.UTCTime ->
  Environment.Flow API.Types.RiderPlatform.IssueManagement.IssueList.IssueListRes
getIssueListV1 merchantShortId opCity apiTokenInfo limit offset mobileCountryCode mobileNumber from to = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.IssueManagement.callIssueManagementAPI
    checkedMerchantId
    opCity
    (.issueListDSL.getIssueListV1)
    limit
    offset
    mobileCountryCode
    mobileNumber
    from
    to

postIssueListTicketStatusCallBack ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Data.Aeson.Value ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postIssueListTicketStatusCallBack merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.IssueManagement.callIssueManagementAPI
    checkedMerchantId
    opCity
    (.issueListDSL.postIssueListTicketStatusCallBack)
    req
