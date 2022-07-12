{-# OPTIONS_GHC -Wno-unused-matches #-}

module Product.DriveronBoarding.DriverOnBoarding where
import Environment
import Domain.Types.Driveronboarding.OperatingCity as DO
import Beckn.Types.Id
import qualified Storage.Queries.Driveronboarding.OperatingCity as QOC
import Domain.Types.Driveronboarding.OperatingCity as DOP
import qualified EulerHS.Language as L
import Beckn.Storage.Esqueleto
import Utils.Common
import Prelude
import Beckn.Prelude
import qualified Domain.Types.Person as SP
import Beckn.Types.Error (PersonError(PersonFieldNotPresent, PersonNotFound), OrganizationError (OrgNotFound))
import qualified Storage.Queries.Organization as QOrganization
import qualified Storage.Queries.Person as QPerson
import Types.API.Driveronboarding.DriverOnBoarding
import Beckn.Types.APISuccess

registrationHandler1 :: Id SP.Person -> DriverOnBoardingReq -> FlowHandler DriverOnBoardingRes
registrationHandler1 personId request@DriverOnBoardingReq {..}= withFlowHandlerAPI $ do
    person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
    orgId <- person.organizationId & fromMaybeM (PersonFieldNotPresent "organization_id")
    organization <-QOrganization.findById orgId
      >>= fromMaybeM (OrgNotFound orgId.getId)
    let orgTxt = getId organization.id    
    opId <- L.generateGUID
    utcNow <- getCurrentTime
    runTransaction $
        QOC.create (mkDBOP opId orgTxt request utcNow)
    return Success


-- sendData :: Id SP.Person -> OperatingCityReq -> FlowHandler OperatingCityRes
-- sendData personId request@OperatingCityReq {..} = withFlowHandlerAPI $ do
--     person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
--     orgId <- person.organizationId & fromMaybeM (PersonFieldNotPresent "organization_id")
--     organization <-QOrganization.findById orgId
--       >>= fromMaybeM (OrgNotFound orgId.getId)
--     let orgTxt = getId organization.id    
--     opId <- L.generateGUID
--     utcNow <- getCurrentTime
--     runTransaction $
--         Queries.create (mkDBOP opId orgTxt request utcNow)
--     return APISuccess.Success

mkDBOP :: Text -> Text -> DriverOnBoardingReq -> UTCTime -> DOP.OperatingCity
mkDBOP opId orgId req time = 
    DOP.OperatingCity
    {
      id = Id opId,
      organizationId = Id orgId,
      cityName = req.operatingCity,
      enabled = VALID,
      createdAt = time, 
      updatedAt = time  
    }   