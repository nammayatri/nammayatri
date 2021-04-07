{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Beckn.Product.BusinessRule where

import Beckn.Types.Common
import Beckn.Types.Error
import qualified Beckn.Utils.Common as Common
import Control.Monad.Except (MonadError, throwError)
import EulerHS.Prelude

-- TODO: replace with Log.Log
class BusinessLog m where
  logTagDebug :: Text -> Text -> m ()
  logTagInfo :: Text -> Text -> m ()
  logTagWarning :: Text -> Text -> m ()
  logTagError :: Text -> Text -> m ()

instance BusinessLog (FlowR r) where
  logTagDebug = Common.logTagDebug
  logTagInfo = Common.logTagInfo
  logTagWarning = Common.logTagWarning
  logTagError = Common.logTagError

instance (Monad m, BusinessLog m) => BusinessLog (BusinessRule m) where
  logTagDebug code msg = lift $ logTagDebug code msg
  logTagInfo code msg = lift $ logTagInfo code msg
  logTagWarning code msg = lift $ logTagWarning code msg
  logTagError code msg = lift $ logTagError code msg

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
    Left be -> do
      let msg = "Error happened when evaluating business rule. Error: " +|| be ||+ ""
      logTagError "BusinessRule" msg
      Common.throwErrorWithInfo CommonInternalError msg
    Right a -> pure a

runBRFlowMaybe :: (Monad m, BusinessLog m) => BusinessRule m a -> m (Maybe a)
runBRFlowMaybe br =
  runBR br >>= \case
    Left be -> do
      logTagError
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
