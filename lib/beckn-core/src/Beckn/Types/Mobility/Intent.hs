{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Mobility.Intent where

import Beckn.Types.Core.Price
import Beckn.Types.Core.Scalar
import Beckn.Types.Core.Tag
import Beckn.Types.Mobility.Payload
import Beckn.Types.Mobility.Stop
import Beckn.Types.Mobility.Vehicle
import Beckn.Utils.Example
import Data.OpenApi (ToSchema)
import Data.Text
import EulerHS.Prelude

data Intent = Intent
  { query_string :: Maybe Text,
    provider_id :: Maybe Text,
    category_id :: Maybe Text,
    item_id :: Maybe Text,
    tags :: Maybe [Tag],
    -- Mobility specific
    pickups :: [Stop],
    drops :: [Stop],
    vehicle :: Vehicle,
    payload :: Payload,
    transfer :: Maybe TransferAttrs,
    fare :: Price
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data TransferAttrs = TransferAttrs
  { max_count :: Int,
    distance :: Scalar
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

instance Example Intent where
  example =
    Intent
      { query_string = Nothing,
        provider_id = Nothing,
        category_id = Nothing,
        item_id = Nothing,
        tags = Just example,
        pickups = example,
        drops = example,
        vehicle = example,
        payload = example,
        transfer = Nothing,
        fare = example
      }
