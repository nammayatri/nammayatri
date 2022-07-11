module Product.DriveronBoarding.Status where

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

-- mkDBOP :: Text -> Text -> OperatingCityReq -> UTCTime -> DOP.OperatingCity
-- mkDBOP opId orgId OperatingCityReq {..} time = 
--     DOP.OperatingCity
--     {
--       id = Id opId,
--       organizationId = Id orgId,
--       cityName = operatingCity,
--       enabled = True,
--       createdAt = time, 
--       updatedAt = time  
--     }   

-- sendData :: Id SP.Person -> FlowHandler OperatingCityRes
-- sendData personId = 