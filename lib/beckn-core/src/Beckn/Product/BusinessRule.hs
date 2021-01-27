{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Beckn.Product.BusinessRule where

import Beckn.Types.Common
import Beckn.Utils.Common
import Control.Monad.Except (MonadError, throwError)
import qualified EulerHS.Language as L
import EulerHS.Prelude

class BusinessLog m where
  logDebug :: Text -> Text -> m ()
  logInfo :: Text -> Text -> m ()
  logWarning :: Text -> Text -> m ()
  logError :: Text -> Text -> m ()

instance BusinessLog (FlowR r) where
  logDebug = L.logDebug
  logInfo = L.logInfo
  logWarning = L.logWarning
  logError = L.logError

instance (Monad m, BusinessLog m) => BusinessLog (BusinessRule m) where
  logDebug code msg = lift $ logDebug code msg
  logInfo code msg = lift $ logInfo code msg
  logWarning code msg = lift $ logWarning code msg
  logError code msg = lift $ logError code msg

newtype BusinessRule m a = BusinessRule {runBusinessRule :: ExceptT BusinessError m a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadTrans,
      MonadError BusinessError,
      MonadThrow,
      MonadCatch,
      MonadMask
    )

data BusinessError = BusinessError
  { errorCode :: Text,
    errorMsg :: Text
  }
  deriving (Show, Eq)

runBR :: BusinessRule m a -> m (Either BusinessError a)
runBR = runExceptT . runBusinessRule

runBRFlowFatal :: HasCallStack => BusinessRule (FlowR r) a -> FlowR r a
runBRFlowFatal br =
  runBR br >>= \case
    Left be@BusinessError {errorCode} -> do
      logError
        "BusinessRule"
        $ "Error happened when evaluating business rule. Error: " +|| be ||+ ""
      throwBecknError500 errorCode
    Right a -> pure a

runBRFlowMaybe :: (Monad m, BusinessLog m) => BusinessRule m a -> m (Maybe a)
runBRFlowMaybe br =
  runBR br >>= \case
    Left be -> do
      logError
        "BusinessRule"
        $ "Error happened when evaluating business rule. Error: " +|| be ||+ ""
      pure Nothing
    Right a -> pure $ Just a

throwBusinessError :: (HasCallStack, Monad m) => Text -> Text -> BusinessRule m a
throwBusinessError code msg = throwError $ BusinessError code msg

fromMaybeBR :: (HasCallStack, Monad m) => Text -> Maybe a -> BusinessRule m a
fromMaybeBR errCode = \case
  Nothing -> throwBusinessError errCode $ getMsgFromCode errCode
  Just a -> pure a

getMsgFromCode :: Text -> Text
getMsgFromCode "NO_FARE_POLICY" = "FarePolicy was not found."
getMsgFromCode "CANT_CALCULATE_DISTANCE" = "Can't calculate the distance."
getMsgFromCode _ = "Something went wrong"
