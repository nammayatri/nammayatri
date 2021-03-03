module Beckn.TypeClass.IsError where

import EulerHS.Prelude

class ToJSON api_error => IsError domain_error api_error where
  toError :: domain_error -> api_error