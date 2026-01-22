{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Management.Transaction
  ( getTransactionList,
    buildTransaction,
    withTransactionStoring,
  )
where

import qualified API.Types.Management.Transaction
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text
import qualified Data.Text as T
import qualified Data.Text.Encoding as DTE
import qualified Domain.Types.AccessMatrix as DMatrix
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.Transaction as DTransaction
import qualified Environment
import EulerHS.Prelude hiding (id, length)
import Kernel.Beam.Functions as B
import Kernel.External.Encryption (decrypt, getDbHash)
import qualified Kernel.External.Encryption
import qualified Kernel.External.Verification.SafetyPortal.Types
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as City
import qualified Kernel.Types.Id
import Kernel.Utils.Common (MonadFlow, encodeToText, generateGUID, getCurrentTime, throwError)
import Storage.Beam.BeamFlow (BeamFlow)
import qualified Storage.Beam.BeamFlow
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Transaction as QTransaction
import qualified Storage.Queries.TransactionExtra as QTransactionExtra
import Tools.Auth.Api
import qualified Tools.Error as E

getTransactionList ::
  Kernel.Types.Id.ShortId DMerchant.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Maybe Data.Text.Text ->
  Maybe Integer ->
  Maybe Integer ->
  Maybe (Kernel.Types.Id.Id DPerson.Person) ->
  Maybe Data.Text.Text ->
  Maybe Data.Text.Text ->
  Maybe DMatrix.UserActionType ->
  Environment.Flow API.Types.Management.Transaction.ListTransactionResp
getTransactionList _ _ _ mbSearchString mbLimit mbOffset mbRequestorId mbDriverId mbRideId mbEndpoint = do
  mbSearchStrDBHash <- getDbHash `traverse` mbSearchString
  transactionList <- B.runInReplica $ QTransactionExtra.findAllTransactionsByLimitOffset mbSearchString mbSearchStrDBHash mbLimit mbOffset mbRequestorId mbDriverId mbRideId mbEndpoint
  transactions <- forM transactionList $ \(transaction, encPerson) -> do
    decPerson <- decrypt encPerson
    pure $ mkTransactionAPIEntity transaction decPerson
  let count = length transactions
  let summary = Kernel.External.Verification.SafetyPortal.Types.Summary {totalCount = 10000, count}
  pure $ API.Types.Management.Transaction.ListTransactionResp {list = transactions, summary = summary}

mkTransactionAPIEntity :: DTransaction.Transaction -> DPerson.DecryptedPerson -> API.Types.Management.Transaction.TransactionAPIEntity
mkTransactionAPIEntity transaction requestor =
  API.Types.Management.Transaction.TransactionAPIEntity
    { id = transaction.id,
      requestor = mkRequestorAPIEntity requestor,
      merchantId = transaction.merchantId,
      endpoint = transaction.endpoint,
      commonDriverId = transaction.commonDriverId,
      commonRideId = transaction.commonRideId,
      request = transaction.request,
      response = transaction.response,
      responseError = transaction.responseError,
      createdAt = transaction.createdAt
    }

mkRequestorAPIEntity :: DPerson.DecryptedPerson -> API.Types.Management.Transaction.RequestorAPIEntity
mkRequestorAPIEntity person =
  API.Types.Management.Transaction.RequestorAPIEntity
    { id = person.id,
      firstName = person.firstName,
      lastName = person.lastName,
      email = person.email,
      mobileNumber = fromMaybe "" person.mobileNumber,
      mobileCountryCode = fromMaybe "" person.mobileCountryCode,
      registeredAt = person.createdAt,
      verified = person.verified
    }

-- Build a transaction record for storing API call information
buildTransaction ::
  (MonadFlow m, ToJSON request) =>
  DMatrix.UserActionType ->
  Maybe DMatrix.ServerName ->
  Maybe ApiTokenInfo ->
  Maybe Data.Text.Text ->
  Maybe Data.Text.Text ->
  Maybe request ->
  m DTransaction.Transaction
buildTransaction endpoint serverName apiTokenInfo commonDriverId commonRideId request = do
  uid <- generateGUID
  now <- getCurrentTime
  pure
    DTransaction.Transaction
      { id = Kernel.Types.Id.Id uid,
        requestorId = apiTokenInfo <&> (.personId),
        merchantId = apiTokenInfo <&> (.merchant.id),
        serverName = serverName,
        endpoint = endpoint,
        commonDriverId = commonDriverId,
        commonRideId = commonRideId,
        request = encodeToText <$> request,
        response = Nothing,
        responseError = Nothing,
        merchantOperatingCityId = Nothing,
        createdAt = now,
        updatedAt = now
      }

emptyResponse :: Maybe ()
emptyResponse = Nothing

-- | Run client call and store transaction to DB.
--
-- If client call fails, then write error code to transaction.
withTransactionStoring ::
  (BeamFlow m r, MonadCatch m) =>
  DTransaction.Transaction ->
  m response ->
  m response
withTransactionStoring =
  withResponseTransactionStoring' (const emptyResponse)

withResponseTransactionStoring' ::
  (BeamFlow m r, MonadCatch m, ToJSON transactionResponse) =>
  (response -> Maybe transactionResponse) ->
  DTransaction.Transaction ->
  m response ->
  m response
withResponseTransactionStoring' responseModifier transaction clientCall = handle errorHandler $ do
  response <- clientCall
  QTransaction.create $ transaction{response = encodeToText <$> responseModifier response}
  pure response
  where
    -- This code do not handle ExternalAPICallError, only E.Error
    errorHandler (err :: E.Error) = do
      QTransaction.create transaction{responseError = Just $ show err}
      throwError err
