{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Product.FareCalculator.BusinessRule where

import Control.Monad.Except (MonadError, throwError)
import EulerHS.Prelude

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

throwBusinessError :: (HasCallStack, Monad m) => Text -> Text -> BusinessRule m a
throwBusinessError code msg = throwError $ BusinessError code msg

fromMaybeBR :: (HasCallStack, Monad m) => Text -> Maybe a -> BusinessRule m a
fromMaybeBR errCode = \case
  Nothing -> throwBusinessError errCode $ getMsgFromCode errCode
  Just a -> pure a

getMsgFromCode :: Text -> Text
getMsgFromCode "NO_FARE_CONFIG" = "FareConfig was not found."
getMsgFromCode _ = "Something went wrong"
