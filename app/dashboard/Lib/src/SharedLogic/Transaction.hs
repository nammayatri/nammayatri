module SharedLogic.Transaction where

import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Utils.Common (encodeToText, throwError)
import qualified "dashboard-bpp-helper-api" Dashboard.Common as Common
import qualified Domain.Types.Transaction as DTransaction
import qualified Storage.Queries.Transaction as QTransaction
import Tools.Auth
import qualified Tools.Error as E

emptyRequest :: Maybe ()
emptyRequest = Nothing

emptyResponse :: Maybe ()
emptyResponse = Nothing

buildTransaction ::
  ( MonadFlow m,
    ToJSON request
  ) =>
  DTransaction.Endpoint ->
  ApiTokenInfo ->
  Id Common.Driver ->
  Maybe request ->
  m DTransaction.Transaction
buildTransaction endpoint apiTokenInfo commonDriverId request = do
  uid <- generateGUID
  now <- getCurrentTime
  pure
    DTransaction.Transaction
      { id = uid,
        personId = apiTokenInfo.personId,
        merchantId = Just apiTokenInfo.merchant.id,
        commonDriverId = Just commonDriverId,
        commonRideId = Nothing,
        request = encodeToText <$> request,
        response = Nothing,
        responseError = Nothing,
        createdAt = now,
        ..
      }

withTransactionStoring :: (Esq.EsqDBFlow m r, MonadCatch m) => DTransaction.Transaction -> m a -> m a
withTransactionStoring transaction clientCall = handle errorHandler $ do
  res <- clientCall
  Esq.runTransaction $
    QTransaction.create transaction
  pure res
  where
    -- This code do not handle ExternalAPICallError, only E.Error
    errorHandler (err :: E.Error) = do
      Esq.runTransaction $
        QTransaction.create transaction{responseError = Just $ show err}
      throwError err
