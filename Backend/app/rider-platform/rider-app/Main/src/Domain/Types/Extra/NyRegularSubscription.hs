{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
-- Added for orphan instance
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Extra.NyRegularSubscription where

-- For Data.Aeson.String
-- Assuming this exports Text, pure, $, Maybe(..), mempty, map, show, (.)
import Control.Lens ((&), (?~)) -- For lens operators
import Data.Aeson (Value (String))
import Data.OpenApi (NamedSchema (..), OpenApiType (OpenApiString), ToSchema (..), enum_, type_)
-- Make sure DayOfWeek constructors (Monday..) are in scope
import qualified Data.Text as T -- For T.pack
import Data.Time.Calendar (DayOfWeek (..))
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import Servant.API (FromHttpApiData (..))

-- If Kernel.Prelude doesn't export some basics like show, map:
-- import Prelude (show, map, Maybe(..), ($), (.), pure, mempty)

-- Extra code goes here --

instance ToSchema DayOfWeek where
  declareNamedSchema _ =
    pure $
      NamedSchema (Just "DayOfWeek") $
        mempty
          & type_ ?~ OpenApiString
          & enum_ ?~ (map (Data.Aeson.String . T.pack . show) [Monday .. Sunday])

data NyRegularSubscriptionStatus = NEW | ACTIVE | PAUSED | CANCELLED | EXPIRED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

instance FromHttpApiData NyRegularSubscriptionStatus where
  parseUrlPiece = pure . read . T.unpack

$(mkBeamInstancesForEnumAndList ''NyRegularSubscriptionStatus)
