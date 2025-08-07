{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.AttractionRecommend where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.TicketPlace
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

data Attraction = Attraction {distanceInKm :: Kernel.Prelude.Double, iconUrl :: Kernel.Prelude.Maybe Kernel.Prelude.Text, id :: Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace, name :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AttractionRecommendReq = AttractionRecommendReq {count :: Kernel.Prelude.Int, lat :: Kernel.Prelude.Double, lon :: Kernel.Prelude.Double, radius :: Kernel.Prelude.Double}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AttractionRecommendResp = AttractionRecommendResp {attractions :: [Attraction]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
