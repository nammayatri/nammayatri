module Domain.Types.VSTAllowedArea
  ( VSTAllowedArea (..),
    vstAllowedAreaToText,
    vstAllowedAreaFromText,
  )
where

import Data.Aeson
import qualified Data.Text as T
import Kernel.Prelude
import Kernel.Types.Id
import Lib.Types.SpecialLocation (SpecialLocation)

data VSTAllowedArea = VSTAllowedArea
  { pickupId :: Id SpecialLocation,
    dropId :: Id SpecialLocation
  }
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

vstAllowedAreaToText :: VSTAllowedArea -> Text
vstAllowedAreaToText area =
  "PickupDrop_" <> area.pickupId.getId <> "_" <> area.dropId.getId

vstAllowedAreaFromText :: Text -> Maybe VSTAllowedArea
vstAllowedAreaFromText t = do
  rest <- T.stripPrefix "PickupDrop_" t
  let (pId, dropPart) = T.breakOn "_" rest
  dId <- T.stripPrefix "_" dropPart
  if T.null pId || T.null dId
    then Nothing
    else Just VSTAllowedArea {pickupId = Id pId, dropId = Id dId}
