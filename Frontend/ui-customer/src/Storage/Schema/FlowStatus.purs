module Storage.Schema.FlowStatus where

import Prelude
import Engineering.Helpers.SQLiteUtils
import Services.API as ApiTypes
import Data.Maybe
import Data.Array as DA

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
dummyFlowStatusColumns :: FlowStatusColumns
dummyFlowStatusColumns = 
    {
        flow_id : ""
      , valid_till : ""
      , flow_type : ""
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

transformFromFlowStatusResp :: ApiTypes.FlowStatusRes -> Array FlowStatusColumns
transformFromFlowStatusResp (ApiTypes.FlowStatusRes resp) = 
    let flowStatus = resp.currentStatus
        createFlowStatusColumns flowId validTill flowType = 
            [{ flow_id : flowId
            , valid_till : validTill
            , flow_type : flowType
            }]
    in case flowStatus of
        ApiTypes.IDLE _ -> 
            createFlowStatusColumns "" "" "IDLE"
        ApiTypes.SEARCHING currentStatus -> 
            createFlowStatusColumns currentStatus.requestId currentStatus.validTill "SEARCHING"
        ApiTypes.GOT_ESTIMATE currentStatus -> 
            createFlowStatusColumns currentStatus.requestId currentStatus.validTill "GOT_ESTIMATE"
        ApiTypes.WAITING_FOR_DRIVER_OFFERS currentStatus -> 
            createFlowStatusColumns currentStatus.estimateId currentStatus.validTill "WAITING_FOR_DRIVER_OFFERS"
        ApiTypes.DRIVER_OFFERED_QUOTE currentStatus -> 
            createFlowStatusColumns currentStatus.estimateId currentStatus.validTill "DRIVER_OFFERED_QUOTE"
        ApiTypes.WAITING_FOR_DRIVER_ASSIGNMENT currentStatus -> 
            createFlowStatusColumns currentStatus.bookingId currentStatus.validTill "WAITING_FOR_DRIVER_ASSIGNMENT"
        ApiTypes.RIDE_ASSIGNED currentStatus -> 
            createFlowStatusColumns currentStatus.rideId "" "RIDE_ASSIGNED"
        ApiTypes.PENDING_RATING currentStatus -> 
            createFlowStatusColumns currentStatus.rideId "" "PENDING_RATING"

transformFromFlowsTable :: Array FlowStatusColumns -> ApiTypes.FlowStatusRes
transformFromFlowsTable flowStatusArr = 
    let flowStatus = fromMaybe dummyFlowStatusColumns $ flowStatusArr DA.!! 0
        flowType = flowStatus
    in case flowType.flow_type of
        "IDLE" -> 
            ApiTypes.FlowStatusRes 
                { currentStatus : ApiTypes.IDLE {}
                , oldStatus : Nothing
                , isValueAddNP : Nothing
                }
        "SEARCHING" -> ApiTypes.FlowStatusRes 
                            { currentStatus : ApiTypes.SEARCHING 
                                { requestId : flowStatus.flow_id
                                , validTill : flowStatus.valid_till
                                }
                            , oldStatus : Nothing
                            , isValueAddNP : Nothing
                            }
        "GOT_ESTIMATE" -> 
            ApiTypes.FlowStatusRes 
                { currentStatus : ApiTypes.GOT_ESTIMATE 
                    { requestId : flowStatus.flow_id
                    , validTill : flowStatus.valid_till
                    }
                , oldStatus : Nothing
                , isValueAddNP : Nothing
                }
        "WAITING_FOR_DRIVER_OFFERS" -> 
            ApiTypes.FlowStatusRes 
                { currentStatus : ApiTypes.WAITING_FOR_DRIVER_OFFERS 
                    { estimateId : flowStatus.flow_id
                    , validTill : flowStatus.valid_till
                    , otherSelectedEstimates : Nothing
                    }
                , oldStatus : Nothing
                , isValueAddNP : Nothing
                }
        "DRIVER_OFFERED_QUOTE" -> 
            ApiTypes.FlowStatusRes 
                { currentStatus : ApiTypes.DRIVER_OFFERED_QUOTE 
                    { estimateId : flowStatus.flow_id
                    , validTill : flowStatus.valid_till
                    }
                , oldStatus : Nothing
                , isValueAddNP : Nothing
                }
        "WAITING_FOR_DRIVER_ASSIGNMENT" -> 
            ApiTypes.FlowStatusRes 
                { currentStatus : ApiTypes.WAITING_FOR_DRIVER_ASSIGNMENT 
                    { bookingId : flowStatus.flow_id
                    , validTill : flowStatus.valid_till
                    , fareProductType : Nothing
                    }
                , oldStatus : Nothing
                , isValueAddNP : Nothing
                }
        "RIDE_ASSIGNED" -> 
            ApiTypes.FlowStatusRes 
                { currentStatus : ApiTypes.RIDE_ASSIGNED 
                    { rideId : flowStatus.flow_id
                    }
                , oldStatus : Nothing
                , isValueAddNP : Nothing
                }
        "PENDING_RATING" -> 
            ApiTypes.FlowStatusRes 
                { currentStatus : ApiTypes.PENDING_RATING 
                    { rideId : flowStatus.flow_id
                    }
                , oldStatus : Nothing
                , isValueAddNP : Nothing
                }
        _ -> 
            ApiTypes.FlowStatusRes 
                { currentStatus : ApiTypes.IDLE {}
                , oldStatus : Nothing
                , isValueAddNP : Nothing
                }