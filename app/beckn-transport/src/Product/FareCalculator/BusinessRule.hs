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

throwBusinessError :: Monad m => Text -> Text -> BusinessRule m a
throwBusinessError code msg = throwError $ BusinessError code msg
