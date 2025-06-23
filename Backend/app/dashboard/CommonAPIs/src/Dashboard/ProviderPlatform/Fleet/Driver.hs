{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Dashboard.ProviderPlatform.Fleet.Driver
  ( module Dashboard.ProviderPlatform.Fleet.Driver,
    module Reexport,
  )
where

import API.Types.ProviderPlatform.Fleet.Endpoints.Driver as Reexport
import Dashboard.Common as Reexport
import Dashboard.Common.Driver as Reexport
import Data.Text as T
import Kernel.Prelude
import Kernel.ServantMultipart
import Kernel.Types.Predicate
import qualified Kernel.Utils.Predicates as P
import Kernel.Utils.Validation

validateAddVehicleReq :: Validate AddVehicleReq
validateAddVehicleReq AddVehicleReq {..} =
  sequenceA_
    [ validateField "registrationNo" registrationNo $
        LengthInRange 1 11 `And` star (P.latinUC \/ P.digit)
    ]

-- Create Drivers using csv --

instance FromMultipart Tmp CreateDriversReq where
  fromMultipart form = do
    fileData <- lookupFile "file" form
    let fleetOwnerId = lookupInput "fleetOwnerId" form
    pure $
      CreateDriversReq
        { file = fdPayload fileData,
          fleetOwnerId = case fleetOwnerId of
            Left _ -> Nothing
            Right x -> Just x
        }

instance ToMultipart Tmp CreateDriversReq where
  toMultipart form = do
    let inputArr = case form.fleetOwnerId of
          Nothing -> []
          Just id -> [Input "fleetOwnerId" id]
    MultipartData inputArr [FileData "file" (T.pack form.file) "" form.file]

-- Create Vehicles using csv --

instance FromMultipart Tmp CreateVehiclesReq where
  fromMultipart form = do
    fileData <- lookupFile "file" form
    let fleetOwnerId = lookupInput "fleetOwnerId" form
        requestorId = lookupInput "requestorId" form
    pure $
      CreateVehiclesReq
        { file = fdPayload fileData,
          fleetOwnerId = case fleetOwnerId of
            Left _ -> Nothing
            Right x -> Just x,
          requestorId = case requestorId of
            Left _ -> Nothing
            Right x -> Just x
        }

instance ToMultipart Tmp CreateVehiclesReq where
  toMultipart form = do
    let inputArr =
          [ ("fleetOwnerId", form.fleetOwnerId),
            ("requestorId", form.requestorId)
          ]
            & mapMaybe (\(k, mv) -> fmap (Input k) mv)
    MultipartData inputArr [FileData "file" (T.pack form.file) "" (form.file)]

-- Vehicle Driver Route mapping --

instance FromMultipart Tmp CreateDriverBusRouteMappingReq where
  fromMultipart form = do
    fileData <- lookupFile "file" form
    let fleetOwnerId = lookupInput "fleetOwnerId" form
    pure $
      CreateDriverBusRouteMappingReq
        { file = fdPayload fileData,
          fleetOwnerId = case fleetOwnerId of
            Left _ -> Nothing
            Right x -> Just x
        }

instance ToMultipart Tmp CreateDriverBusRouteMappingReq where
  toMultipart form = do
    let inputArr = case form.fleetOwnerId of
          Nothing -> []
          Just id -> [Input "fleetOwnerId" id]
    MultipartData inputArr [FileData "file" (T.pack form.file) "" (form.file)]
