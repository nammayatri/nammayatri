module Temporary.Validation where

import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Utils.Common
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Types.Beckn.Context

-- Use Beckn.Product.Validation.Context after migration to 0.9

validateCountry :: (L.MonadFlow m, Log m) => Context -> m ()
validateCountry context =
  unless (context.country == Just "IND") $
    throwError InvalidCountry

validateCity :: (L.MonadFlow m, Log m) => Context -> m ()
validateCity context =
  -- just for testing purposes, to be rewritten later as well as country check
  unless (isJust $ context.country) $
    throwError InvalidCity

validateAction :: (L.MonadFlow m, Log m) => Text -> Context -> m ()
validateAction expectedAction context =
  unless (context.action == expectedAction) $
    throwError InvalidAction
