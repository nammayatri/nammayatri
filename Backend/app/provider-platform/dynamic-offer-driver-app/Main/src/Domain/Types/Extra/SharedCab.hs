{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Types.Extra.SharedCab where

import API.Types.UI.SharedCab (SelectRouteMode (..), SharedCabRouteOperationReq)
import Data.Aeson (withText)
import EulerHS.Prelude hiding (id)

-- contract: mode?: 'afterLastDrop' | 'force'
instance ToJSON SelectRouteMode where
  toJSON AfterLastDrop = "afterLastDrop"
  toJSON Force = "force"

instance FromJSON SelectRouteMode where
  parseJSON = withText "SelectRouteMode" $ \case
    "afterLastDrop" -> pure AfterLastDrop
    "force" -> pure Force
    tag -> fail $ "Invalid select-route mode: " <> show tag

instance ToJSON SharedCabRouteOperationReq

instance FromJSON SharedCabRouteOperationReq
