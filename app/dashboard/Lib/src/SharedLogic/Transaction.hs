module SharedLogic.Transaction where

import Beckn.Prelude
import Beckn.Types.Common
import Beckn.Types.Id
import qualified "dashboard-bpp-helper-api" Dashboard.Common as Common
import qualified Domain.Types.Transaction as DTransaction
import Tools.Auth

buildTransaction ::
  MonadFlow m =>
  ApiTokenInfo ->
  Id Common.Driver ->
  DTransaction.Endpoint ->
  Maybe Text ->
  Maybe Text ->
  m DTransaction.Transaction
buildTransaction apiTokenInfo commonDriverId endpoint request errorCode = do
  uid <- generateGUID
  now <- getCurrentTime
  pure
    DTransaction.Transaction
      { id = uid,
        personId = apiTokenInfo.personId,
        merchantId = Just apiTokenInfo.merchant.id,
        commonDriverId = Just commonDriverId,
        commonRideId = Nothing,
        response = Nothing,
        createdAt = now,
        ..
      }
