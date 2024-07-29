{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Dashboard.ProviderPlatform.Fleet.Driver
  ( module Dashboard.ProviderPlatform.Fleet.Driver,
    module Reexport,
  )
where

import API.Types.ProviderPlatform.Fleet.Driver as Reexport
import Dashboard.Common as Reexport
import Dashboard.Common.Driver as Reexport
import Data.Aeson
import Kernel.Prelude
import Kernel.Types.Predicate
import qualified Kernel.Utils.Predicates as P
import Kernel.Utils.TH (mkHttpInstancesForEnum)
import Kernel.Utils.Validation

validateAddVehicleReq :: Validate AddVehicleReq
validateAddVehicleReq AddVehicleReq {..} =
  sequenceA_
    [ validateField "color" colour $ NotEmpty `And` P.name,
      validateField "registrationNo" registrationNo $
        LengthInRange 1 11 `And` star (P.latinUC \/ P.digit)
    ]

$(mkHttpInstancesForEnum ''FleetVehicleStatus)

$(mkHttpInstancesForEnum ''DriverMode)

$(mkHttpInstancesForEnum ''SortOn)
