module Domain.Types.Extra.Quote where

import Domain.Types.Quote (TollChargesInfo (..))
import Kernel.Types.Common (Price)

getTollChargesInfoPrice :: TollChargesInfo -> Price
getTollChargesInfoPrice TollChargesInfo {tollCharges} = tollCharges
