{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.FarePolicy.FarePolicySlabsDetails
  ( module Reexport,
    module Domain.Types.FarePolicy.FarePolicySlabsDetails,
  )
where

import Control.Lens.Combinators
import Control.Lens.Fold
import qualified Data.Aeson.Key as DAK
import qualified Data.Aeson.KeyMap as DAKM
import Data.Aeson.Lens
import Data.Aeson.Types
import qualified Data.List.NonEmpty as NE
import Data.Ord
import qualified Data.Text as Text
import Domain.Types.Common
import Domain.Types.FarePolicy.FarePolicySlabsDetails.FarePolicySlabsDetailsSlab as Reexport
import Kernel.Prelude
import Kernel.Types.Cac
import Kernel.Types.Common

newtype FPSlabsDetailsD (s :: UsageSafety) = FPSlabsDetails
  { slabs :: NonEmpty (FPSlabsDetailsSlabD s)
  }
  deriving (Generic, Show, Eq)

type FPSlabsDetails = FPSlabsDetailsD 'Safe

instance ToJSON (FPSlabsDetailsD 'Unsafe)

instance FromJSON (FPSlabsDetailsD 'Unsafe)

instance FromJSON (FPSlabsDetailsD 'Safe)

instance ToJSON (FPSlabsDetailsD 'Safe)

findFPSlabsDetailsSlabByDistance :: Meters -> NonEmpty (FPSlabsDetailsSlabD s) -> FPSlabsDetailsSlabD s
findFPSlabsDetailsSlabByDistance dist slabList = do
  case NE.filter (\slab -> slab.startDistance <= dist) $ NE.sortBy (comparing (.startDistance)) slabList of
    [] -> error $ "Slab for dist = " <> show dist <> " not found. Non-emptiness supposed to be guaranteed by app logic."
    a -> last a

-----------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------APIEntity--------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------

newtype FPSlabsDetailsAPIEntity = FPSlabsDetailsAPIEntity
  { slabs :: NonEmpty FPSlabsDetailsSlabAPIEntity
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

getFPSlabDetailsSlab :: String -> String -> Maybe FPSlabsDetails
getFPSlabDetailsSlab config key' = do
  let k =
        config
          ^@.. _Value
            . _Object
            . reindexed
              (dropPrefixFromConfig "farePolicySlabsDetailsSlab:")
              ( itraversed
                  . indices
                    ( Text.isPrefixOf
                        "farePolicySlabsDetailsSlab:"
                        . DAK.toText
                    )
              )
      fpsdsl = jsonToFPSlabsDetailsSlab (DAKM.fromList k) key'
  case NE.nonEmpty fpsdsl of
    Just fpsdsl' -> Just (FPSlabsDetails fpsdsl')
    Nothing -> Nothing

makeFPSlabsDetailsAPIEntity :: FPSlabsDetails -> FPSlabsDetailsAPIEntity
makeFPSlabsDetailsAPIEntity FPSlabsDetails {..} =
  FPSlabsDetailsAPIEntity
    { slabs = makeFPSlabsDetailsSlabAPIEntity <$> slabs
    }
