module Product.Validation where

import App.Types
import Beckn.Product.Validation.Context
import Beckn.Types.Core.Context
import EulerHS.Prelude

validateContext :: Text -> Context -> Flow ()
validateContext = validateContextCommons
