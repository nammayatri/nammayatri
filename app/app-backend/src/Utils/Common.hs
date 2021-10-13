{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Utils.Common
  ( module Utils.Common,
    module CoreCommon,
  )
where

import Beckn.Product.Validation.Context
import Beckn.Types.Common
import Beckn.Types.Core.Context
import Beckn.Types.Core.Domain
-- import qualified Beckn.Types.Core.Migration.Fulfillment as Mig

-- import qualified Beckn.Types.Core.Migration.Time as Mig
import qualified Beckn.Types.Core.Migration.Gps as Mig
import qualified Beckn.Types.Core.Migration.Intent as Mig
import qualified Beckn.Types.Core.Migration.Location as Mig
import Beckn.Types.Id
import Beckn.Types.Mobility.Intent
import Beckn.Types.Mobility.Payload
import Beckn.Utils.Common as CoreCommon
import Control.Lens ((?~))
import Data.Text as DT
import qualified Data.Text as T
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified Test.RandomStrings as RS
import qualified Types.API.Search as API
import Types.Common
import Types.Error

generateShortId :: MonadFlow m => m (ShortId a)
generateShortId = ShortId . T.pack <$> L.runIO (RS.randomString (RS.onlyAlphaNum RS.randomASCII) 10)

mkIntent :: API.SearchReq -> Intent
mkIntent req =
  Intent
    { query_string = Nothing,
      provider_id = Nothing,
      category_id = Nothing,
      item_id = Nothing,
      tags = Nothing,
      pickups = [toBeckn (req.origin)],
      drops = [toBeckn (req.destination)],
      vehicle = Nothing,
      payload = Payload Nothing Nothing [] Nothing,
      transfer = Nothing,
      fare = toBeckn $ req.fare
    }

mkIntentMig :: (MonadThrow m, Log m) => API.SearchReq -> m Mig.Intent
mkIntentMig req = do
  from <- stopToLoc req.origin
  to <- stopToLoc req.destination
  pure $
    Mig.emptyIntent
      & #fulfillment
        ?~ ( Mig.emptyFulFillmentInfo
               & #start
                 ?~ Mig.LocationAndTime
                   { location = Just from,
                     time = Nothing
                   }
               & #end
                 ?~ Mig.LocationAndTime
                   { location = Just to,
                     time = Nothing
                   }
           )
  where
    stopToLoc Stop {location} = do
      GPS {lat, lon} <- location.gps & fromMaybeM (InvalidRequest "no gps field")
      gps <-
        fromMaybeM (InvalidRequest "bad coordinates") $
          Mig.Gps
            <$> readMaybe (T.unpack lat)
            <*> readMaybe (T.unpack lon)
      pure $ Mig.emptyLocation & #gps ?~ gps

{-
            "time": {
                "label": "start_time",
                "timestamp": "2021-09-29T09:55:41.161Z"
            }
-}

validateContext :: (HasFlowEnv m r ["coreVersion" ::: Text, "domainVersion" ::: Text]) => Text -> Context -> m ()
validateContext action context = do
  validateDomain MOBILITY context
  validateContextCommons action context
