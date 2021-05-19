{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Utils.Common
  ( module Utils.Common,
    module CoreCommon,
  )
where

import App.Types
import Beckn.Product.Validation.Context
import Beckn.Types.Common
import Beckn.Types.Core.Context
import Beckn.Types.Core.Domain
import Beckn.Types.Mobility.Intent
import Beckn.Types.Mobility.Payload
import Beckn.Utils.Common as CoreCommon
import Data.Text as DT
import qualified Data.Text as T
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified Test.RandomStrings as RS
import qualified Types.API.Search as API

generateShortId :: Flow Text
generateShortId = T.pack <$> L.runIO (RS.randomString (RS.onlyAlphaNum RS.randomASCII) 10)

mkIntent :: API.SearchReq -> Intent
mkIntent req =
  Intent
    { _query_string = Nothing,
      _provider_id = Nothing,
      _category_id = Nothing,
      _item_id = Nothing,
      _tags = Nothing,
      _pickups = [toBeckn (req ^. #origin)],
      _drops = [toBeckn (req ^. #destination)],
      _vehicle = toBeckn $ req ^. #vehicle,
      _payload = Payload Nothing Nothing [] Nothing,
      _transfer = Nothing,
      _fare = toBeckn $ req ^. #fare
    }

validateContext :: Text -> Context -> Flow ()
validateContext action context = do
  validateDomain MOBILITY context
  validateContextCommons action context
