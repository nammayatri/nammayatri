module Beckn.OnDemand.Utils.OnInit where

import qualified BecknV2.OnDemand.Types as Spec
import qualified Data.Aeson as A
import Data.Text as T
import qualified Domain.Types.Location as DLoc
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Gps as Gps

mkStops :: DLoc.Location -> Maybe DLoc.Location -> Maybe [Spec.Stop]
mkStops origin mbDestination =
  let originGps = Gps.Gps {lat = origin.lat, lon = origin.lon}
      destinationGps dest = Gps.Gps {lat = dest.lat, lon = dest.lon}
   in Just $
        catMaybes
          [ Just $
              Spec.Stop
                { stopLocation =
                    Just $
                      Spec.Location
                        { locationAddress = Just $ mkAddress origin.address,
                          locationAreaCode = origin.address.areaCode,
                          locationCity = Just $ Spec.City Nothing origin.address.city,
                          locationCountry = Just $ Spec.Country Nothing origin.address.country,
                          locationGps = A.decode $ A.encode originGps,
                          locationState = Just $ Spec.State origin.address.state,
                          locationId = Nothing
                        },
                  stopType = Just "START",
                  stopAuthorization = Nothing,
                  stopTime = Nothing
                },
            ( \destination ->
                Spec.Stop
                  { stopLocation =
                      Just $
                        Spec.Location
                          { locationAddress = Just $ mkAddress destination.address,
                            locationAreaCode = destination.address.areaCode,
                            locationCity = Just $ Spec.City Nothing destination.address.city,
                            locationCountry = Just $ Spec.Country Nothing destination.address.country,
                            locationGps = A.decode $ A.encode $ destinationGps destination,
                            locationState = Just $ Spec.State destination.address.state,
                            locationId = Nothing
                          },
                    stopType = Just "END",
                    stopAuthorization = Nothing,
                    stopTime = Nothing
                  }
            )
              <$> mbDestination
          ]
  where
    mkAddress :: DLoc.LocationAddress -> Text
    mkAddress DLoc.LocationAddress {..} = T.intercalate ", " $ catMaybes [door, building, street]
