{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module MockData.PlaceName (mkMockPlaceNameResp) where

import Kernel.External.Maps.Google.MapsClient.Types as GoogleMaps
import qualified Kernel.External.Maps.Types as Maps
import Kernel.Prelude

mkMockPlaceNameResp :: Maps.LatLong -> GoogleMaps.GetPlaceNameResp
mkMockPlaceNameResp latLng =
  GoogleMaps.GetPlaceNameResp
    { status = "OK",
      results = [mkMockResultsResp latLng]
    }

mkMockResultsResp :: Maps.LatLong -> GoogleMaps.ResultsResp
mkMockResultsResp latLng =
  GoogleMaps.ResultsResp
    { formatted_address = Just "Davangere, Davangere-Harihar Rd, M B Kere, Jalinagar, Davanagere, Karnataka 577001, India",
      address_components = mockAddressComponents,
      plus_code =
        Just
          GoogleMaps.PlusCodeResp
            { compound_code = Just "FW99+8R Davanagere, Karnataka, India",
              global_code = Nothing
            },
      geometry =
        GoogleMaps.Geometry
          { location =
              GoogleMaps.LocationS
                { lat = latLng.lat,
                  lng = latLng.lon
                }
          },
      place_id = Nothing
    }

mockAddressComponents :: [GoogleMaps.AddressResp]
mockAddressComponents =
  [ GoogleMaps.AddressResp
      { long_name = "Davangere",
        short_name = "Davangere",
        types =
          [ "establishment",
            "point_of_interest",
            "train_station",
            "transit_station"
          ]
      },
    GoogleMaps.AddressResp
      { long_name = "M B Kere",
        short_name = "M B Kere",
        types =
          [ "political",
            "sublocality",
            "sublocality_level_2"
          ]
      },
    GoogleMaps.AddressResp
      { long_name = "Jalinagar",
        short_name = "Jalinagar",
        types =
          [ "political",
            "sublocality",
            "sublocality_level_1"
          ]
      },
    GoogleMaps.AddressResp
      { long_name = "Davanagere",
        short_name = "Davanagere",
        types =
          [ "locality",
            "political"
          ]
      },
    GoogleMaps.AddressResp
      { long_name = "Davanagere",
        short_name = "Davanagere",
        types =
          [ "administrative_area_level_3",
            "political"
          ]
      },
    GoogleMaps.AddressResp
      { long_name = "Karnataka",
        short_name = "KA",
        types =
          [ "administrative_area_level_1",
            "political"
          ]
      },
    GoogleMaps.AddressResp
      { long_name = "India",
        short_name = "IN",
        types =
          [ "country",
            "political"
          ]
      },
    GoogleMaps.AddressResp
      { long_name = "577001",
        short_name = "577001",
        types =
          [ "postal_code"
          ]
      }
  ]
