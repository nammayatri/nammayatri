module Product.DriveronBoarding.OperatingCity where
import Types.API.Driveronboarding.OperatingCity 
import App.Types (FlowHandler)
import Domain.Types.Driveronboarding.OperatingCity as DO
import Beckn.Types.Id
import qualified Storage.Queries.Driveronboarding.OperatingCity as Queries
import Domain.Types.Driveronboarding.OperatingCity as DOP
import qualified Beckn.Types.APISuccess as APISuccess
import qualified EulerHS.Language as L
import Beckn.Storage.Esqueleto
import Utils.Common
import Prelude
import Beckn.Prelude
import qualified Domain.Types.Person as SP
import Beckn.Types.Error (PersonError(PersonFieldNotPresent, PersonNotFound), OrganizationError (OrgNotFound))
import qualified Storage.Queries.Organization as QOrganization
import qualified Storage.Queries.Person as QPerson

-- registrationHandler :: OperatingCityReq -> FlowHandler APISuccess
-- registrationHandler req = do
--     when (isJust req.operatingCity) $ do
--         validate req.operatingCity
    

-- something :: OperatingCity
-- something = DO.cityName


-- data OperatingCityReq = OperatingCityReq{
--     organization_id :: Text,
--     vehicleRegistrationCertNumber :: Maybe Text,
--     driverLicenseNumber :: Maybe Text,
--     operatingCity :: Text
-- }  

sendData :: Id SP.Person -> OperatingCityReq -> FlowHandler OperatingCityRes
sendData personId request@OperatingCityReq {..} = withFlowHandlerAPI $ do
    person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
    orgId <- person.organizationId & fromMaybeM (PersonFieldNotPresent "organization_id")
    organization <-QOrganization.findById orgId
      >>= fromMaybeM (OrgNotFound orgId.getId)
    let orgTxt = getId organization.id    
    opId <- L.generateGUID
    utcNow <- getCurrentTime
    runTransaction $
        Queries.create (mkDBOP opId orgTxt request utcNow)
    return APISuccess.Success

mkDBOP :: Text -> Text -> OperatingCityReq -> UTCTime -> DOP.OperatingCity
mkDBOP opId orgId OperatingCityReq {..} time = 
    DOP.OperatingCity
    {
      id = Id opId,
      organizationId = Id orgId,
      cityName = operatingCity,
      enabled = True,
      createdAt = time, 
      updatedAt = time  
    }   

