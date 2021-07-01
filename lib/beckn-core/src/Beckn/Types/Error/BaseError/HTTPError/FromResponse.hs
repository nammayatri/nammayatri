module Beckn.Types.Error.BaseError.HTTPError.FromResponse where

import qualified Data.Aeson as A
import EulerHS.Prelude
import Servant.Client (Response, ResponseF (Response))

class FromResponse e where
  fromResponse :: Response -> Maybe e

fromJsonResponse :: FromJSON a => Response -> Maybe a
fromJsonResponse (Response _ _ _ body) = A.decode body
