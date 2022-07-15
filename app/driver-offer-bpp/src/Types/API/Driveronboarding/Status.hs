module Types.API.Driveronboarding.Status where
import Beckn.Prelude
import Domain.Types.Driveronboarding.OperatingCity


data ResponseStatus = PENDINGVERIFICATION | VERIFIED | FAILEDVERIFICATION | WAITING_INPUT 
    deriving ( Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema, Enum, Bounded)

data StatusRes = StatusRes
    {
        dlVerificationStatus :: ResponseStatus,
        rcVerificationStatus :: ResponseStatus,
        operatingCityStatus :: OperatingCityVerification
    }
    deriving ( Show,Eq,Read,Generic,ToJSON,FromJSON,ToSchema)
