-- | Supported city centers + the service-area geofence — verbatim port of
-- @ny-connectors/connectors/src/ny/cities.ts@. Used to (a) snap the autocomplete
-- search center to the nearest city (@findNearestCity@ + 'citySearchRadiusMeters')
-- and (b) decide whether a flexi pickup pin is within the merchant's service area
-- ('isWithinServiceArea', FAIL-OPEN on an unknown area name).
module WhatsappBot.Cities
  ( CityCenter (..),
    supportedCities,
    defaultCity,
    citySearchRadiusMeters,
    haversineKm,
    findNearestCity,
    cityCenterByName,
    isWithinServiceArea,
  )
where

import qualified Data.Text as T
import Kernel.Prelude

data CityCenter = CityCenter
  { name :: Text,
    lat :: Double,
    lon :: Double
  }
  deriving (Show, Eq, Generic)

-- | @SUPPORTED_CITIES@ (@cities.ts:12-129@), in order (index 0 = the default).
supportedCities :: [CityCenter]
supportedCities =
  [ -- Karnataka
    CityCenter "Bangalore" 12.9716 77.5946,
    CityCenter "Mysore" 12.2958 76.6394,
    CityCenter "Tumakuru" 13.3409 77.1010,
    CityCenter "Hubli" 15.3647 75.1240,
    CityCenter "Mangalore" 12.9141 74.8560,
    CityCenter "Udupi" 13.3409 74.7421,
    CityCenter "Davanagere" 14.4644 75.9218,
    CityCenter "Shivamogga" 13.9299 75.5681,
    CityCenter "Gulbarga" 17.3297 76.8343,
    CityCenter "Bidar" 17.9133 77.5301,
    CityCenter "Ballari" 15.1394 76.9214,
    -- Tamil Nadu
    CityCenter "Chennai" 13.0827 80.2707,
    CityCenter "Coimbatore" 11.0168 76.9558,
    CityCenter "Madurai" 9.9252 78.1198,
    CityCenter "Salem" 11.6643 78.1460,
    CityCenter "Hosur" 12.7409 77.8253,
    CityCenter "Trichy" 10.7905 78.7047,
    CityCenter "Tirunelveli" 8.7139 77.7567,
    CityCenter "Thanjavur" 10.7870 79.1378,
    CityCenter "Vellore" 12.9165 79.1325,
    CityCenter "Pudukkottai" 10.3833 78.8001,
    CityCenter "Mayiladuthurai" 11.1018 79.6515,
    -- Telangana
    CityCenter "Hyderabad" 17.3850 78.4867,
    CityCenter "Warangal" 17.9689 79.5941,
    CityCenter "Khammam" 17.2473 80.1514,
    CityCenter "Karimnagar" 18.4386 79.1288,
    CityCenter "Nizamabad" 18.6725 78.0941,
    CityCenter "Mahbubnagar" 16.7393 77.9974,
    CityCenter "Suryapet" 17.1346 79.6228,
    CityCenter "Nalgonda" 17.0542 79.2671,
    CityCenter "Siddipet" 18.1018 78.8474,
    -- Andhra Pradesh
    CityCenter "Vijayawada" 16.5062 80.6480,
    CityCenter "Vishakapatnam" 17.6868 83.2185,
    CityCenter "Guntur" 16.3067 80.4365,
    CityCenter "Tirupati" 13.6288 79.4192,
    CityCenter "Kurnool" 15.8281 78.0373,
    -- Kerala
    CityCenter "Kochi" 9.9312 76.2673,
    CityCenter "Trivandrum" 8.5241 76.9366,
    CityCenter "Thrissur" 10.5276 76.2144,
    CityCenter "Kozhikode" 11.2588 75.7804,
    CityCenter "Alapuzha" 9.4981 76.3388,
    CityCenter "Idukki" 9.8497 76.9700,
    CityCenter "Kasaragod" 12.4996 74.9869,
    CityCenter "Wayanad" 11.6854 76.1320,
    CityCenter "Kannur" 11.8745 75.3704,
    CityCenter "Kottayam" 9.5916 76.5222,
    CityCenter "Palakkad" 10.7867 76.6548,
    CityCenter "Kollam" 8.8932 76.6141,
    CityCenter "Pathanamthitta" 9.2648 76.7870,
    CityCenter "Malappuram" 11.0510 76.0711,
    -- West Bengal
    CityCenter "Kolkata" 22.5726 88.3639,
    CityCenter "Siliguri" 26.7271 88.3953,
    CityCenter "Asansol" 23.6889 86.9661,
    CityCenter "Durgapur" 23.5204 87.3119,
    CityCenter "Petrapole" 23.0070 88.7980,
    CityCenter "Darjeeling" 27.0360 88.2627,
    CityCenter "Bardhaman" 23.2324 87.8615,
    CityCenter "PurbaBardhaman" 23.2324 87.8615,
    CityCenter "Birbhum" 23.8408 87.6193,
    CityCenter "Bankura" 23.2480 87.0680,
    CityCenter "Digha" 21.6276 87.5089,
    -- Odisha
    CityCenter "Bhubaneshwar" 20.2961 85.8245,
    CityCenter "Cuttack" 20.4625 85.8830,
    CityCenter "Puri" 19.8135 85.8312,
    CityCenter "Rourkela" 22.2604 84.8536,
    CityCenter "Berhampur" 19.3149 84.7941,
    CityCenter "Jharsuguda" 21.8554 84.0062,
    CityCenter "Sambalpur" 21.4669 83.9756,
    -- Maharashtra
    CityCenter "Mumbai" 19.0760 72.8777,
    CityCenter "Pune" 18.5204 73.8567,
    -- Gujarat
    CityCenter "Ahmedabad" 23.0225 72.5714,
    CityCenter "Surat" 21.1702 72.8311,
    CityCenter "Vadodara" 22.3072 73.1812,
    CityCenter "Rajkot" 22.3039 70.8022,
    CityCenter "Jamnagar" 22.4707 70.0577,
    CityCenter "Somnath" 20.8880 70.4011,
    CityCenter "Dwarka" 22.2442 68.9685,
    -- North India
    CityCenter "Delhi" 28.6139 77.2090,
    CityCenter "Noida" 28.5355 77.3910,
    CityCenter "Gurugram" 28.4595 77.0266,
    CityCenter "Chandigarh" 30.7333 76.7794,
    CityCenter "Jaipur" 26.9124 75.7873,
    -- Jammu & Kashmir
    CityCenter "Srinagar" 34.0837 74.7973,
    CityCenter "Pulwama" 33.8716 74.8949,
    CityCenter "Jammu" 32.7266 74.8570,
    CityCenter "Anantnag" 33.7311 75.1487,
    -- Northeast & Sikkim
    CityCenter "Gangtok" 27.3389 88.6065,
    CityCenter "Shillong" 25.5788 91.8933,
    CityCenter "Cherrapunji" 25.2702 91.7323,
    -- Pondicherry
    CityCenter "Pondicherry" 11.9416 79.8083
  ]

-- | Fallback when no location context is available (@cities.ts:132@).
defaultCity :: CityCenter
defaultCity = case supportedCities of
  (c : _) -> c
  [] -> CityCenter "Bangalore" 12.9716 77.5946 -- unreachable; list is non-empty

-- | Autocomplete search radius, meters (@cities.ts:135@).
citySearchRadiusMeters :: Int
citySearchRadiusMeters = 50000

-- | Haversine distance in km (@cities.ts:138-147@).
haversineKm :: Double -> Double -> Double -> Double -> Double
haversineKm lat1 lon1 lat2 lon2 =
  let toRad deg = deg * pi / 180
      r = 6371
      dLat = toRad (lat2 - lat1)
      dLon = toRad (lon2 - lon1)
      a =
        sin (dLat / 2) ** 2
          + cos (toRad lat1) * cos (toRad lat2) * sin (dLon / 2) ** 2
   in 2 * r * asin (sqrt a)

-- | Nearest supported city; if the nearest is > @maxKm@ (200 in TS) away, the
-- default city instead (@cities.ts:153-168@).
findNearestCity :: Double -> Double -> CityCenter
findNearestCity lat0 lon0 =
  let maxKm = 200
      dists = [(c, haversineKm lat0 lon0 c.lat c.lon) | c <- supportedCities]
      (nearest, nearestDist) = minimumBy (\x y -> compare (snd x) (snd y)) dists
   in if nearestDist <= maxKm then nearest else defaultCity

-- | Display-name aliases -> canonical @SUPPORTED_CITIES@ names (@cities.ts:173-181@).
cityAliases :: [(Text, Text)]
cityAliases =
  [ ("tumkur", "Tumakuru"),
    ("tumakuru", "Tumakuru"),
    ("bengaluru", "Bangalore"),
    ("bangalore", "Bangalore"),
    ("mysuru", "Mysore"),
    ("mysore", "Mysore"),
    ("bombay", "Mumbai")
  ]

-- | Resolve a service-area display name to a city center, alias-aware
-- (@cities.ts:184-189@).
cityCenterByName :: Text -> Maybe CityCenter
cityCenterByName nm
  | T.null (T.strip nm) = Nothing
  | otherwise =
    let key = T.toLower (T.strip nm)
        canonical = T.toLower (fromMaybe (T.strip nm) (lookup key cityAliases))
     in find (\c -> T.toLower c.name == canonical) supportedCities

-- | Is a coordinate within @radiusKm@ of the named area's center? FAIL-OPEN:
-- unknown area name -> serviceable (@cities.ts:194-203@).
isWithinServiceArea :: Double -> Double -> Text -> Double -> Bool
isWithinServiceArea lat0 lon0 areaName radiusKm =
  case cityCenterByName areaName of
    Nothing -> True
    Just center -> haversineKm lat0 lon0 center.lat center.lon <= radiusKm
