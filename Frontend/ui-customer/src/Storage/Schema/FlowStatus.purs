module Storage.Schema.FlowStatus where

import Prelude
import Engineering.Helpers.SQLiteUtils
import Services.API as ApiTypes
import Data.Maybe

flowStatusSchema :: SqlSchema
flowStatusSchema =
  [ { "key" : "id", "type" : "integer primary key autoincrement" }
  , { "key" : "flow_type", "type" : "string" }
  , { "key" : "flow_id", "type" : "string" }
  , { "key" : "valid_till", "type" : "string" }
  ]

type FlowStatusColumns = 
    {
        flow_id :: String
      , valid_till :: String
      , flow_type :: String
    }

-- newtype FlowStatusRes = FlowStatusRes
--   { currentStatus :: FlowStatus
--   , oldStatus :: Maybe FlowStatus
--   }

-- data FlowStatus = IDLE {}
--                 | SEARCHING { requestId :: String , validTill :: String }
--                 | GOT_ESTIMATE { requestId :: String , validTill :: String }
--                 | WAITING_FOR_DRIVER_OFFERS { validTill :: String , estimateId :: String }
--                 | DRIVER_OFFERED_QUOTE { validTill :: String , estimateId :: String }
--                 | WAITING_FOR_DRIVER_ASSIGNMENT { bookingId :: String , validTill :: String }
--                 | RIDE_ASSIGNED { rideId :: String }
--                 | PENDING_RATING { rideId :: String }

transformFromFlowStatusResp :: ApiTypes.FlowStatusRes -> FlowStatusColumns
transformFromFlowStatusResp (ApiTypes.FlowStatusRes resp) = 
    let flowStatus = resp.currentStatus
    in case flowStatus of
        ApiTypes.IDLE _ -> 
            { flow_id : ""
            , valid_till : ""
            , flow_type : "IDLE"
            }
        ApiTypes.SEARCHING currentStatus -> 
            { flow_id : currentStatus.requestId
            , valid_till : currentStatus.validTill
            , flow_type : "SEARCHING"
            }
        ApiTypes.GOT_ESTIMATE currentStatus -> 
            { flow_id : currentStatus.requestId
            , valid_till : currentStatus.validTill
            , flow_type : "GOT_ESTIMATE"
            }
        ApiTypes.WAITING_FOR_DRIVER_OFFERS currentStatus -> 
            { flow_id : currentStatus.estimateId
            , valid_till : currentStatus.validTill
            , flow_type : "WAITING_FOR_DRIVER_OFFERS"
            }
        ApiTypes.DRIVER_OFFERED_QUOTE currentStatus -> 
            { flow_id : currentStatus.estimateId
            , valid_till : currentStatus.validTill
            , flow_type : "DRIVER_OFFERED_QUOTE"
            }
        ApiTypes.WAITING_FOR_DRIVER_ASSIGNMENT currentStatus -> 
            { flow_id : currentStatus.bookingId
            , valid_till : currentStatus.validTill
            , flow_type : "WAITING_FOR_DRIVER_ASSIGNMENT"
            }
        ApiTypes.RIDE_ASSIGNED currentStatus -> 
            { flow_id : currentStatus.rideId
            , valid_till : ""
            , flow_type : "RIDE_ASSIGNED"
            }
        ApiTypes.PENDING_RATING currentStatus -> 
            { flow_id : currentStatus.rideId
            , valid_till : ""
            , flow_type : "PENDING_RATING"
            }

transformFromFlowsTable :: FlowStatusColumns -> ApiTypes.FlowStatusRes
transformFromFlowsTable flowStatus = 
    let flowType = flowStatus
    in case flowType.flow_type of
        "IDLE" -> 
            ApiTypes.FlowStatusRes 
                { currentStatus : ApiTypes.IDLE {}
                , oldStatus : Nothing
                }
        "SEARCHING" -> ApiTypes.FlowStatusRes 
                            { currentStatus : ApiTypes.SEARCHING 
                                { requestId : flowStatus.flow_id
                                , validTill : flowStatus.valid_till
                                }
                            , oldStatus : Nothing
                            }
        "GOT_ESTIMATE" -> 
            ApiTypes.FlowStatusRes 
                { currentStatus : ApiTypes.GOT_ESTIMATE 
                    { requestId : flowStatus.flow_id
                    , validTill : flowStatus.valid_till
                    }
                , oldStatus : Nothing
                }
        "WAITING_FOR_DRIVER_OFFERS" -> 
            ApiTypes.FlowStatusRes 
                { currentStatus : ApiTypes.WAITING_FOR_DRIVER_OFFERS 
                    { estimateId : flowStatus.flow_id
                    , validTill : flowStatus.valid_till
                    }
                , oldStatus : Nothing
                }
        "DRIVER_OFFERED_QUOTE" -> 
            ApiTypes.FlowStatusRes 
                { currentStatus : ApiTypes.DRIVER_OFFERED_QUOTE 
                    { estimateId : flowStatus.flow_id
                    , validTill : flowStatus.valid_till
                    }
                , oldStatus : Nothing
                }
        "WAITING_FOR_DRIVER_ASSIGNMENT" -> 
            ApiTypes.FlowStatusRes 
                { currentStatus : ApiTypes.WAITING_FOR_DRIVER_ASSIGNMENT 
                    { bookingId : flowStatus.flow_id
                    , validTill : flowStatus.valid_till
                    }
                , oldStatus : Nothing
                }
        "RIDE_ASSIGNED" -> 
            ApiTypes.FlowStatusRes 
                { currentStatus : ApiTypes.RIDE_ASSIGNED 
                    { rideId : flowStatus.flow_id
                    }
                , oldStatus : Nothing
                }
        "PENDING_RATING" -> 
            ApiTypes.FlowStatusRes 
                { currentStatus : ApiTypes.PENDING_RATING 
                    { rideId : flowStatus.flow_id
                    }
                , oldStatus : Nothing
                }
        _ -> 
            ApiTypes.FlowStatusRes 
                { currentStatus : ApiTypes.IDLE {}
                , oldStatus : Nothing
                }