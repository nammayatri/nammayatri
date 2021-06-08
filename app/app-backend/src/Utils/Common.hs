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
import Beckn.Types.Id
import Beckn.Types.Mobility.Intent
import Beckn.Types.Mobility.Payload
import Beckn.Utils.Common as CoreCommon
import Data.Text as DT
import qualified Data.Text as T
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified Test.RandomStrings as RS
import qualified Types.API.Search as API

generateShortId :: Flow (ShortId a)
generateShortId = ShortId . T.pack <$> L.runIO (RS.randomString (RS.onlyAlphaNum RS.randomASCII) 10)

mkIntent :: API.SearchReq -> Intent
mkIntent req =
  Intent
    { query_string = Nothing,
      provider_id = Nothing,
      category_id = Nothing,
      item_id = Nothing,
      tags = Nothing,
      pickups = [toBeckn (req ^. #origin)],
      drops = [toBeckn (req ^. #destination)],
      vehicle = toBeckn $ req ^. #vehicle,
      payload = Payload Nothing Nothing [] Nothing,
      transfer = Nothing,
      fare = toBeckn $ req ^. #fare
    }

validateContext :: Text -> Context -> Flow ()
validateContext action context = do
  validateDomain MOBILITY context
  validateContextCommons action context
