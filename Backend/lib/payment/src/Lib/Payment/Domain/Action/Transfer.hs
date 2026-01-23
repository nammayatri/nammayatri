module Lib.Payment.Domain.Action.Transfer
  ( createTransferService,
    transferProccessingKey,
  )
where

import qualified Kernel.External.Payment.Interface.Types as PInterface
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Payment.Domain.Types.Common
import qualified Lib.Payment.Domain.Types.TransferTransaction as DTransfer
import Lib.Payment.Storage.Beam.BeamFlow
import qualified Lib.Payment.Storage.Queries.TransferTransaction as QTransfer

-- | Create a Stripe transfer and persist it in the 'TransferTransaction' table.
--   The 'entityId' field of 'TransferTransaction' is used to link the transfer to a domain entity (via 'TransferEntityName').
createTransferService ::
  forall m r c.
  ( EncFlow m r,
    BeamFlow m r,
    HasShortDurationRetryCfg r c
  ) =>
  Id Merchant ->
  Id MerchantOperatingCity ->
  DTransfer.TransferEntityName ->
  Id DTransfer.TransferEntity ->
  PInterface.CreateTransferReq ->
  (PInterface.CreateTransferReq -> m PInterface.CreateTransferResp) ->
  m (Either Text PInterface.CreateTransferResp)
createTransferService merchantId merchantOperatingCityId entityName entityId createTransferReq createTransferCall = do
  Redis.whenWithLockRedisAndReturnValue (transferProccessingKey entityId) 60 processTransfer' >>= \case
    Left () -> return . Left $ "Transfer already initiated: " <> entityId.getId
    Right (response :: Either Text PInterface.CreateTransferResp) -> return response
  where
    processTransfer' :: m (Either Text PInterface.CreateTransferResp)
    processTransfer' =
      try processTransfer >>= \case
        Right resp -> pure $ Right resp
        Left err -> pure $ Left (getErrMessage err)

    getErrMessage :: SomeException -> Text
    getErrMessage exc
      | Just (BaseException baseExc) <- fromException @BaseException exc = fromMaybe "Failed to initiate transfer: " $ toMessage baseExc
      | otherwise = "Failed to initiate transfer"

    processTransfer :: m PInterface.CreateTransferResp
    processTransfer = do
      transferTxn <- buildTransferTxn
      QTransfer.create transferTxn
      createTransferResp <- withTryCatch "createRefundsCall:refundService" $ withShortRetry $ createTransferCall createTransferReq
      case createTransferResp of
        Right response -> do
          -- error code and error message present only if api failed
          let isApiCallSuccess = Just True
          QTransfer.updateTranferEntryByStripeResponse (Just response.transferId.getTransferId) Nothing Nothing (Just response.status) isApiCallSuccess transferTxn.id
          pure response
        Left err -> do
          logError $ "Create Refund API Call Failure with Error: " <> show err
          let mbStripeError = fromException @PInterface.StripeError err
          let isApiCallSuccess = Just False
          QTransfer.updateTranferEntryByStripeResponse transferTxn.idAssignedByServiceProvider (mbStripeError >>= getStripeErrorCode) (mbStripeError >>= getStripeErrorMessage) (Just PInterface.TRANSFER_FAILED) isApiCallSuccess transferTxn.id
          throwError $ InvalidRequest "Create Refund API call error"

    buildTransferTxn :: m DTransfer.TransferTransaction
    buildTransferTxn = do
      now <- getCurrentTime
      txnId <- generateGUID
      let senderAccountId = Just createTransferReq.senderConnectedAccountId
          destinationAccountId =
            case createTransferReq.destinationAccount of
              PInterface.TransferConnectedAccount accountId -> Just accountId
              PInterface.TransferPlatformAccount -> Nothing
      pure
        DTransfer.TransferTransaction
          { id = Id txnId,
            entityId,
            idAssignedByServiceProvider = Nothing,
            status = Nothing,
            isApiCallSuccess = Nothing,
            amount = createTransferReq.amount,
            currency = createTransferReq.currency,
            senderAccountId,
            destinationAccountId,
            description = createTransferReq.description,
            entityName = entityName,
            errorCode = Nothing,
            errorMessage = Nothing,
            responseDump = Nothing, -- can be populated in webhook
            merchantId,
            merchantOperatingCityId,
            createdAt = now,
            updatedAt = now
          }

transferProccessingKey :: Id DTransfer.TransferEntity -> Text
transferProccessingKey transferEntityId = "Transfer:Processing:TransferId:" <> transferEntityId.getId

getStripeErrorCode :: PInterface.StripeError -> Maybe Text
getStripeErrorCode = \case
  PInterface.ApiError info -> info.errorCode
  PInterface.CardError info -> info.errorCode
  PInterface.IdempotencyError info -> info.errorCode
  PInterface.InvalidRequestError info -> info.errorCode
  PInterface.SomethingWentWrong _ -> Nothing

getStripeErrorMessage :: PInterface.StripeError -> Maybe Text
getStripeErrorMessage = \case
  PInterface.ApiError info -> info.errorMessage
  PInterface.CardError info -> info.errorMessage
  PInterface.IdempotencyError info -> info.errorMessage
  PInterface.InvalidRequestError info -> info.errorMessage
  PInterface.SomethingWentWrong _ -> Nothing
