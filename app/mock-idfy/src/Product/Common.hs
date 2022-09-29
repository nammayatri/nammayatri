module Product.Common where

import Beckn.Tools.Metrics.CoreMetrics
import Beckn.Utils.Common hiding (Error)
import EulerHS.Prelude
import qualified EulerHS.Types as T
import Types.API.VerifyDLAsync
import Types.API.VerifyRCAsync
import Types.Common
import Types.IdfyRes

callIdfyAPI :: CallAPI env res
callIdfyAPI = callApiUnwrappingApiError (identity @Error) Nothing Nothing

verifyRC ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  ApiKey ->
  AccountId ->
  BaseUrl ->
  VerifyRCReq ->
  m IdfyRes
verifyRC apiKey accountId url req = callIdfyAPI url task "verifyRC"
  where
    task =
      T.client
        verifyRCAPI
        (Just apiKey)
        (Just accountId)
        req

verifyDL ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  ApiKey ->
  AccountId ->
  BaseUrl ->
  VerifyDLReq ->
  m IdfyRes
verifyDL apiKey accountId url req = callIdfyAPI url task "verifyDL"
  where
    task =
      T.client
        verifyDLAPI
        (Just apiKey)
        (Just accountId)
        req
