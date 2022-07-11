
module Domain.Types.Driveronboarding.VehicleRegistrationCert where
import Beckn.Prelude
import Beckn.Types.Id
import Domain.Types.Person (Person)

-- 3-T , - _
data VehicleClass = TW_NT | TW_T | TW_CAB | HGV_T | HMV_HGV | HMV | HTV | LMV | LMV_NT | LMV_T | LMV_CAB | LMV_HMV | LTV | MCWG | MCWOG | HPMV | MGV | MMV | LDRXCV | PSV_BUS | TRANS | TRCTOR | Others
    deriving (Show,Eq,Read,Generic,ToJSON,FromJSON,Enum,Bounded,ToSchema)
-- here we should only check vehicle class with three wheeler vehicle type only

data Verification2 = PENDINGVERIFICATION | VERIFIED | FAILEDVERIFICATION | WAITINGINPUT deriving
    ( Show,
      Eq,
      Read,
      Generic,
      ToJSON,
      FromJSON,
      ToSchema,
      ToParamSchema,
      Enum,
      Bounded
    )

    
data VehicleRegistrationCert = VehicleRegistrationCert {
    id :: Id VehicleRegistrationCert,
    driverId :: Id Person,
    vehicleRegistrationCertNumber :: Maybe Text,
    fitnessCertExpiry :: Maybe UTCTime,
    permitNumber :: Maybe Text,
    permitStart :: Maybe UTCTime,
    permitExpiry :: Maybe UTCTime,
    vehicleClass :: Maybe VehicleClass,
    vehicleRegStatus :: Verification2,
    vehicleNumber :: Maybe Text,
    request_id :: Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
}






