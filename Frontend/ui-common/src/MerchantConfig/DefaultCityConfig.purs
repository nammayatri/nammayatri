module MerchantConfig.DefaultCityConfig where

import MerchantConfig.Types
import Common.Types.Config 

config :: CityConfigs
config 
  = { cities : 
        [{
          cityName : "Bangalore",
          mapImage : "ny_ic_bengalore_map",
          cityCode : "std:080",
          showSubscriptions : true,
          cityLat : 12.971599,
          cityLong : 77.594566,
          supportNumber : "",
          languageKey : "KN_IN"
        },
        {
          cityName : "Hyderabad",
          mapImage : "ny_ic_hyderabad_map",
          cityCode : "std:040",
          showSubscriptions : false,
          cityLat : 17.387140,
          cityLong : 78.491684,
          supportNumber : "+918069724900",
          languageKey : "TE_IN"
        },
        {
          cityName : "Mysore",
          mapImage : "ny_ic_mysuru_map",
          cityCode : "std:0821",
          showSubscriptions : false,
          cityLat : 12.295810,
          cityLong : 76.639381,
          supportNumber : "",
          languageKey : "TA_IN"
        },
        {
          cityName : "Delhi",
          mapImage : "ny_ic_delhi_map",
          cityCode : "std:011",
          showSubscriptions : false,
          cityLat : 28.644800,
          cityLong : 77.216721,
          supportNumber : "+918069724848",
          languageKey : "HI_IN"
        },
        {
          cityName : "Chennai",
          mapImage : "ny_ic_chennai_map",
          cityCode : "std:044",
          showSubscriptions : false,
          cityLat : 13.067439,
          cityLong : 80.237617,
          supportNumber : "08069724899",
          languageKey : "TA_IN"
        },
        {
          cityName : "Coimbatore",
          mapImage : "ny_ic_coimbatore_map",
          cityCode : "std:0422",
          showSubscriptions : false,
          cityLat : 11.004556,
          cityLong : 76.961632,
          supportNumber : "",
          languageKey : "TA_IN"
        },
        {
          cityName : "Puducherry",
          mapImage : "ny_ic_puducherry_map",
          cityCode : "std:0413",
          showSubscriptions : false,
          cityLat : 11.943852,
          cityLong : 79.808292,
          supportNumber : "08069724899",
          languageKey : "TA_IN"
        }
    ]}