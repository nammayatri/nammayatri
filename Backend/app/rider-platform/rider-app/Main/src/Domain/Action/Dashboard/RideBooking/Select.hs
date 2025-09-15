module Domain.Action.Dashboard.RideBooking.Select
  ( postSelectEstimate,
    getSelectQuotes,
    getSelectResult,
    postSelectCancelSearch,
  )
where

import qualified API.UI.CancelSearch
import qualified API.UI.Select
import qualified "this" Domain.Action.UI.Select
import qualified "this" Domain.Types.Estimate
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.Person
import qualified Environment
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import SharedLogic.Cancel
import SharedLogic.Merchant (findMerchantByShortId)

postSelectEstimate ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id Domain.Types.Person.Person ->
  Kernel.Types.Id.Id Domain.Types.Estimate.Estimate ->
  Domain.Action.UI.Select.DSelectReq ->
  Environment.Flow Domain.Action.UI.Select.MultimodalSelectRes
postSelectEstimate merchantShortId _opCity personId estimateId req = do
  m <- findMerchantByShortId merchantShortId
  API.UI.Select.select2' (personId, m.id) estimateId req

getSelectQuotes ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id Domain.Types.Person.Person ->
  Kernel.Types.Id.Id Domain.Types.Estimate.Estimate ->
  Environment.Flow Domain.Action.UI.Select.SelectListRes
getSelectQuotes merchantShortId _opCity personId estimateId = do
  m <- findMerchantByShortId merchantShortId
  API.UI.Select.selectList' (personId, m.id) estimateId

getSelectResult ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id Domain.Types.Person.Person ->
  Kernel.Types.Id.Id Domain.Types.Estimate.Estimate ->
  Environment.Flow Domain.Action.UI.Select.QuotesResultResponse
getSelectResult merchantShortId _opCity personId estimateId = do
  m <- findMerchantByShortId merchantShortId
  API.UI.Select.selectResult' (personId, m.id) estimateId

postSelectCancelSearch ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id Domain.Types.Person.Person ->
  Kernel.Types.Id.Id Domain.Types.Estimate.Estimate ->
  Environment.Flow CancelAPIResponse
postSelectCancelSearch merchantShortId _opCity personId estimateId = do
  m <- findMerchantByShortId merchantShortId
  API.UI.CancelSearch.cancelSearch' (personId, m.id) estimateId
