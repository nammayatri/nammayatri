 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
 
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program 
 
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of 
 
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module MockData.DistanceMatrix where

import qualified Data.Map as Map
import qualified Domain.Types.MockPlace as DPlace
import Kernel.External.Maps.Google.MapsClient
import Kernel.Prelude
import Servant

availablePlaces :: Map.Map DPlace.PlaceId DPlace.MockPlace
availablePlaces =
  Map.fromList . map (\p -> (p.placeId, p)) $
    [ origin1,
      destination1,
      origin2,
      destination2,
      origin3,
      destination3,
      origin4,
      destination4,
      origin5,
      destination5,
      origin6,
      destination6,
      origin7,
      destination7,
      origin13
    ]

distanceMatrixElements :: Map.Map (DPlace.PlaceId, DPlace.PlaceId) DistanceMatrixElement
distanceMatrixElements =
  Map.fromList . map (\(p1, p2, e) -> ((p1.placeId, p2.placeId), e)) $
    [ (origin1, destination1, element1),
      (origin2, destination2, element2),
      (origin3, destination3, element3),
      (origin4, destination4, element4),
      (origin5, destination5, element5),
      (origin6, destination6, element6),
      (origin7, destination7, element7),
      (origin3, origin2, element10),
      (origin7, origin4, element11),
      (origin12, destination12, element12),
      (origin13, destination13, element13),
      (origin14, destination14, element14),
      (origin15, destination15, element15)
    ]

mkDefaultPlace :: Place -> DPlace.MockPlace
mkDefaultPlace place =
  DPlace.MockPlace
    { placeId = 0,
      place,
      placeName = mkDefaultAddress place
    }

mkDefaultAddress :: Place -> Text
mkDefaultAddress = toUrlPiece

defaultElement :: DistanceMatrixElement
defaultElement =
  DistanceMatrixElement
    { distance =
        Just
          TextValue
            { text = "1 m",
              value = 0
            },
      duration =
        Just
          TextValue
            { text = "1 min",
              value = 0
            },
      status = "OK"
    }

-- route 1

origin1 :: DPlace.MockPlace
origin1 =
  DPlace.MockPlace
    { placeId = 1,
      place = Location $ LocationS {lat = 12.9352, lng = 77.6245},
      placeName = "WJPF+3QH, KHB Colony, Koramangala 4th Block, Koramangala, Bengaluru, Karnataka 560034, India"
    }

destination1 :: DPlace.MockPlace
destination1 =
  DPlace.MockPlace
    { placeId = 2,
      place = Location $ LocationS {lat = 12.9063, lng = 77.5857},
      placeName = "WH4P+G7M, 1st Phase, J. P. Nagar, Bengaluru, Karnataka 560078, India"
    }

element1 :: DistanceMatrixElement
element1 =
  DistanceMatrixElement
    { distance =
        Just
          TextValue
            { text = "7.2 km",
              value = 7164
            },
      duration =
        Just
          TextValue
            { text = "24 mins",
              value = 1469
            },
      status = "OK"
    }

-- route 2

origin2 :: DPlace.MockPlace
origin2 =
  DPlace.MockPlace
    { placeId = 3,
      place = Location $ LocationS {lat = 10.021737, lng = 76.306198},
      placeName = "Station Kavala, Edappally - Palarivattom Rd, Opposite St George Church, Edappally, Kochi, Kerala 682024, India"
    }

destination2 :: DPlace.MockPlace
destination2 =
  DPlace.MockPlace
    { placeId = 4,
      place = Location $ LocationS {lat = 10.023880, lng = 76.302642},
      placeName = "MCRRA-02, Manimala Cross Road, Ponekkara, Edappally, Kochi, Kerala 682024, India"
    }

element2 :: DistanceMatrixElement
element2 =
  DistanceMatrixElement
    { distance =
        Just
          TextValue
            { text = "0.7 km",
              value = 688
            },
      duration =
        Just
          TextValue
            { text = "2 mins",
              value = 145
            },
      status = "OK"
    }

-- route 3

origin3 :: DPlace.MockPlace
origin3 =
  DPlace.MockPlace
    { placeId = 5,
      place = Location $ LocationS {lat = 10.030061, lng = 76.309029},
      placeName = "MNRA-116, Vettikkattu Rd, Edappally Toll, Nethaji Nagar, Edappally, Kochi, Kerala 682024, India"
    }

destination3 :: DPlace.MockPlace
destination3 =
  DPlace.MockPlace
    { placeId = 6,
      place = Location $ LocationS {lat = 10.044056, lng = 76.277771},
      placeName = "Aster Medcity, Near, 27VH+H4H, Kuttisahib Rd, South Chittoor, Ernakulam, Kerala 682027, India"
    }

element3 :: DistanceMatrixElement
element3 =
  DistanceMatrixElement
    { distance =
        Just
          TextValue
            { text = "9.1 km",
              value = 9070
            },
      duration =
        Just
          TextValue
            { text = "20 mins",
              value = 1182
            },
      status = "OK"
    }

-- route 4

origin4 :: DPlace.MockPlace
origin4 =
  DPlace.MockPlace
    { placeId = 7,
      place = Location $ LocationS {lat = 14.467318, lng = 75.919232},
      placeName = "Davangere, Davangere-Harihar Rd, M B Kere, Jalinagar, Davanagere, Karnataka 577001, India"
    }

destination4 :: DPlace.MockPlace
destination4 =
  DPlace.MockPlace
    { placeId = 8,
      place = Location $ LocationS {lat = 14.479898, lng = 75.915455},
      placeName = "budal road, S, P.S Nagar, FWH8+W39, Vinayaka Nagara, Davanagere, Karnataka 577004, India"
    }

element4 :: DistanceMatrixElement
element4 =
  DistanceMatrixElement
    { distance =
        Just
          TextValue
            { text = "2.0 km",
              value = 2023
            },
      duration =
        Just
          TextValue
            { text = "8 mins",
              value = 496
            },
      status = "OK"
    }

-- route 5

origin5 :: DPlace.MockPlace
origin5 =
  DPlace.MockPlace
    { placeId = 9,
      place = Location $ LocationS {lat = 14.445983, lng = 75.947267},
      placeName = "L-2666, Ak Colony, Avaragere, Davanagere, Karnataka 577003, India"
    }

destination5 :: DPlace.MockPlace
destination5 =
  DPlace.MockPlace
    { placeId = 10,
      place = Location $ LocationS {lat = 14.472649, lng = 75.888420},
      placeName = "Ground Floor PB Road, Davangere Harihara Road Hotel Road, near Ocean Park, Davanagere, Karnataka 577004, India"
    }

element5 :: DistanceMatrixElement
element5 =
  DistanceMatrixElement
    { distance =
        Just
          TextValue
            { text = "8.6 km",
              value = 8636
            },
      duration =
        Just
          TextValue
            { text = "19 mins",
              value = 1169
            },
      status = "OK"
    }

-- route 6

origin6 :: DPlace.MockPlace
origin6 =
  DPlace.MockPlace
    { placeId = 11,
      place = Location $ LocationS {lat = 10.0739, lng = 76.2733},
      placeName = "37FF+67W, Varapuzha, Kerala 683517, India"
    }

destination6 :: DPlace.MockPlace
destination6 =
  DPlace.MockPlace
    { placeId = 12,
      place = Location $ LocationS {lat = 10.5449, lng = 76.4356},
      placeName = "Karadiyala Rd, Panamkutty, Kizhakkancherry, Kerala 678684, India"
    }

element6 :: DistanceMatrixElement
element6 =
  DistanceMatrixElement
    { distance =
        Just
          TextValue
            { text = "98.0 km",
              value = 97981
            },
      duration =
        Just
          TextValue
            { text = "2 hours 17 mins",
              value = 8229
            },
      status = "OK"
    }

-- route 7

origin7 :: DPlace.MockPlace
origin7 =
  DPlace.MockPlace
    { placeId = 13,
      place = Location $ LocationS {lat = 14.445332, lng = 75.919028},
      placeName = "CWW9+4JF, 60 Feet Rd, Vidyanagar, Davanagere, Karnataka 577005, India"
    }

destination7 :: DPlace.MockPlace
destination7 =
  DPlace.MockPlace
    { placeId = 14,
      place = Location $ LocationS {lat = 14.441555, lng = 75.922259},
      placeName = "600, Hadadi Rd, 2st Stage, Shivakumara Swamy Nagara, Davanagere, Karnataka 577005, India"
    }

element7 :: DistanceMatrixElement
element7 =
  DistanceMatrixElement
    { distance =
        Just
          TextValue
            { text = "0.7 km",
              value = 707
            },
      duration =
        Just
          TextValue
            { text = "2 mins",
              value = 112
            },
      status = "OK"
    }

-- route8 = route7

-- route9 works with empty elements

-- route10

-- origin10 = origin3
-- destination10 = origin2

element10 :: DistanceMatrixElement
element10 =
  DistanceMatrixElement
    { distance =
        Just
          TextValue
            { text = "1.7 km",
              value = 1732
            },
      duration =
        Just
          TextValue
            { text = "6 mins",
              value = 331
            },
      status = "OK"
    }

-- route11

-- origin11 = origin7
-- destination11 = origin4

element11 :: DistanceMatrixElement
element11 =
  DistanceMatrixElement
    { distance =
        Just
          TextValue
            { text = "2.7 km",
              value = 2661
            },
      duration =
        Just
          TextValue
            { text = "8 mins",
              value = 450
            },
      status = "OK"
    }

-- route12

origin12 :: DPlace.MockPlace
origin12 = origin6

destination12 :: DPlace.MockPlace
destination12 = origin2

element12 :: DistanceMatrixElement
element12 =
  DistanceMatrixElement
    { distance =
        Just
          TextValue
            { text = "8.9 km",
              value = 8867
            },
      duration =
        Just
          TextValue
            { text = "21 mins",
              value = 1268
            },
      status = "OK"
    }

-- route13

-- the same as origin6
origin13 :: DPlace.MockPlace
origin13 =
  DPlace.MockPlace
    { placeId = 15,
      place = Location $ LocationS {lat = 10.0741, lng = 76.2733},
      placeName = "37FF+67W, Varapuzha, Kerala 683517, India"
    }

destination13 :: DPlace.MockPlace
destination13 = origin2

element13 :: DistanceMatrixElement
element13 =
  DistanceMatrixElement
    { distance =
        Just
          TextValue
            { text = "8.9 km",
              value = 8867
            },
      duration =
        Just
          TextValue
            { text = "21 mins",
              value = 1268
            },
      status = "OK"
    }

-- route14

origin14 :: DPlace.MockPlace
origin14 = origin6

destination14 :: DPlace.MockPlace
destination14 = origin3

element14 :: DistanceMatrixElement
element14 =
  DistanceMatrixElement
    { distance =
        Just
          TextValue
            { text = "9.4 km",
              value = 9433
            },
      duration =
        Just
          TextValue
            { text = "22 mins",
              value = 1319
            },
      status = "OK"
    }

-- route15

origin15 :: DPlace.MockPlace
origin15 = origin5

destination15 :: DPlace.MockPlace
destination15 = origin4

element15 :: DistanceMatrixElement
element15 =
  DistanceMatrixElement
    { distance =
        Just
          TextValue
            { text = "4.0 km",
              value = 4001
            },
      duration =
        Just
          TextValue
            { text = "10 mins",
              value = 596
            },
      status = "OK"
    }
