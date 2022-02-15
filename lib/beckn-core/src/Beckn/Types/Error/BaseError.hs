module Beckn.Types.Error.BaseError where

import Control.Exception
import EulerHS.Prelude hiding (Show, pack, show)
import Prelude (Show (..))

type IsBaseException e = (IsBaseError e, Exception e)

class IsBaseError e where
  toMessage :: e -> Maybe Text
  toMessage _ = Nothing

data BaseException = forall e. IsBaseException e => BaseException e

instance Show BaseException where
  show (BaseException e) = show e

instance Exception BaseException
