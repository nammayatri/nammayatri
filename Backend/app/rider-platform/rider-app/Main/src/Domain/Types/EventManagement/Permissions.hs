{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Domain.Types.EventManagement.Permissions where

import Control.Lens
import Data.Aeson (Key, Value (..))
import Data.Aeson.Lens
import qualified Domain.Types.EventManagement as DEM
import Kernel.Prelude

needsApproval :: Maybe Value -> DEM.TicketPlaceDef -> DEM.TicketPlaceDef -> Bool
needsApproval Nothing _ _ = False
needsApproval config originalDef changedDef =
  checkBasicInfoChanges originalDef.basicInformation changedDef.basicInformation
    || checkServiceChanges originalDef.services changedDef.services
    || checkCategoryChanges originalDef.serviceCategories changedDef.serviceCategories
    || checkPeopleCategoryChanges originalDef.servicePeopleCategories changedDef.servicePeopleCategories
  where
    basicInfoCheck = basicInfoFieldRequiresApproval config
    serviceCheck = serviceFieldRequiresApproval config
    categoryCheck = categoryFieldRequiresApproval config
    peopleCategoryCheck = peopleCategoryFieldRequiresApproval config

    fieldChanged :: (Eq a) => a -> a -> (Key -> Bool) -> Key -> Bool
    fieldChanged originalVal changedVal permissionCheck fieldName =
      originalVal /= changedVal && permissionCheck fieldName

    checkBasicInfoChanges :: DEM.BasicInformation -> DEM.BasicInformation -> Bool
    checkBasicInfoChanges original changed =
      any
        identity
        [ fieldChanged original.name changed.name basicInfoCheck "name",
          fieldChanged original.description changed.description basicInfoCheck "description",
          fieldChanged original.shortDesc changed.shortDesc basicInfoCheck "shortDesc",
          fieldChanged original.address changed.address basicInfoCheck "address",
          ( fieldChanged original.latitude changed.latitude (const locationCheck) ""
              || fieldChanged original.longitude changed.longitude (const locationCheck) ""
          ),
          fieldChanged original.status changed.status basicInfoCheck "status",
          fieldChanged original.priority changed.priority basicInfoCheck "priority",
          fieldChanged original.placeType changed.placeType basicInfoCheck "placeType",
          fieldChanged original.allowSameDayBooking changed.allowSameDayBooking
            basicInfoCheck
            "allowSameDayBooking",
          fieldChanged original.gallery changed.gallery basicInfoCheck "gallery",
          fieldChanged original.iconUrl changed.iconUrl basicInfoCheck "iconUrl",
          fieldChanged original.mapImageUrl changed.mapImageUrl basicInfoCheck "mapImageUrl",
          fieldChanged original.termsAndConditions changed.termsAndConditions
            basicInfoCheck
            "termsAndConditions",
          fieldChanged original.termsAndConditionsUrl changed.termsAndConditionsUrl
            basicInfoCheck
            "termsAndConditionsUrl",
          fieldChanged original.openTimings changed.openTimings basicInfoCheck "openTimings",
          fieldChanged original.closeTimings changed.closeTimings basicInfoCheck "closeTimings",
          fieldChanged original.customTabs changed.customTabs basicInfoCheck "customTabs"
        ]
      where
        locationCheck = basicInfoFieldRequiresApproval config "location"

    checkServiceChanges :: [DEM.TicketServiceDef] -> [DEM.TicketServiceDef] -> Bool
    checkServiceChanges originalServices changedServices =
      let originalIds = map (.id) originalServices
          changedIds = map (.id) changedServices

          -- Check for added services
          addedServices = filter (\svc -> not (svc.id `elem` originalIds)) changedServices
          addedRequiresApproval = not (null addedServices) && serviceAddRequiresApproval config

          -- Check for deleted services
          deletedServices = filter (\svc -> not (svc.id `elem` changedIds)) originalServices
          deletedRequiresApproval = not (null deletedServices) && serviceDeleteRequiresApproval config

          -- Check for modified services
          modifiedRequiresApproval = any checkServiceChange changedServices
       in addedRequiresApproval || deletedRequiresApproval || modifiedRequiresApproval

    checkServiceChange :: DEM.TicketServiceDef -> Bool
    checkServiceChange changedSvc =
      case find (\s -> s.id == changedSvc.id) originalDef.services of
        Nothing -> False -- already covered by addedRequiresApproval
        Just originalSvc ->
          any
            identity
            [ fieldChanged originalSvc.service changedSvc.service serviceCheck "service",
              fieldChanged originalSvc.shortDesc changedSvc.shortDesc serviceCheck "shortDesc",
              fieldChanged originalSvc.operationalDays changedSvc.operationalDays serviceCheck "operationalDays",
              fieldChanged originalSvc.operationalDate changedSvc.operationalDate serviceCheck "operationalDate",
              fieldChanged originalSvc.maxVerification changedSvc.maxVerification serviceCheck "maxVerification",
              fieldChanged originalSvc.allowFutureBooking changedSvc.allowFutureBooking serviceCheck "allowFutureBooking",
              fieldChanged originalSvc.allowCancellation changedSvc.allowCancellation serviceCheck "allowCancellation",
              fieldChanged originalSvc.expiry changedSvc.expiry serviceCheck "expiry",
              fieldChanged originalSvc.serviceCategoryId changedSvc.serviceCategoryId serviceCheck "serviceCategoryId"
            ]

    checkCategoryChanges :: [DEM.ServiceCategoryDef] -> [DEM.ServiceCategoryDef] -> Bool
    checkCategoryChanges originalCategories changedCategories =
      let originalIds = map (.id) originalCategories
          changedIds = map (.id) changedCategories

          -- Check for added categories
          addedCategories = filter (\cat -> not (cat.id `elem` originalIds)) changedCategories
          addedRequiresApproval = not (null addedCategories) && categoryAddRequiresApproval config

          -- Check for deleted categories
          deletedCategories = filter (\cat -> not (cat.id `elem` changedIds)) originalCategories
          deletedRequiresApproval = not (null deletedCategories) && categoryDeleteRequiresApproval config

          -- Check for modified categories
          modifiedRequiresApproval = any checkCategoryChange changedCategories
       in addedRequiresApproval || deletedRequiresApproval || modifiedRequiresApproval

    checkCategoryChange :: DEM.ServiceCategoryDef -> Bool
    checkCategoryChange changedCat =
      case find (\c -> c.id == changedCat.id) originalDef.serviceCategories of
        Nothing -> False
        Just originalCat ->
          any
            identity
            [ fieldChanged originalCat.name changedCat.name categoryCheck "name",
              fieldChanged originalCat.description changedCat.description categoryCheck "description",
              fieldChanged originalCat.allowedSeats changedCat.allowedSeats categoryCheck "allowedSeats",
              fieldChanged originalCat.businessHours changedCat.businessHours categoryCheck "businessHours",
              fieldChanged originalCat.peopleCategory changedCat.peopleCategory categoryCheck "peopleCategory"
            ]

    checkPeopleCategoryChanges :: [DEM.ServicePeopleCategoryDef] -> [DEM.ServicePeopleCategoryDef] -> Bool
    checkPeopleCategoryChanges originalPeopleCategories changedPeopleCategories =
      let originalIds = map (.id) originalPeopleCategories
          changedIds = map (.id) changedPeopleCategories

          addedPeopleCategories = filter (\pc -> not (pc.id `elem` originalIds)) changedPeopleCategories
          addedRequiresApproval = not (null addedPeopleCategories) && peopleCategoryAddRequiresApproval config

          deletedPeopleCategories = filter (\pc -> not (pc.id `elem` changedIds)) originalPeopleCategories
          deletedRequiresApproval = not (null deletedPeopleCategories) && peopleCategoryDeleteRequiresApproval config

          modifiedRequiresApproval = any checkPeopleCategoryChange changedPeopleCategories
       in addedRequiresApproval || deletedRequiresApproval || modifiedRequiresApproval

    checkPeopleCategoryChange :: DEM.ServicePeopleCategoryDef -> Bool
    checkPeopleCategoryChange changedPC =
      case find (\pc -> pc.id == changedPC.id) originalDef.servicePeopleCategories of
        Nothing -> False
        Just originalPC ->
          any
            identity
            [ fieldChanged originalPC.name changedPC.name peopleCategoryCheck "name",
              fieldChanged originalPC.description changedPC.description peopleCategoryCheck "description",
              fieldChanged originalPC.pricingType changedPC.pricingType peopleCategoryCheck "pricingType",
              fieldChanged originalPC.priceAmount changedPC.priceAmount peopleCategoryCheck "priceAmount",
              fieldChanged originalPC.priceCurrency changedPC.priceCurrency peopleCategoryCheck "priceCurrency"
            ]

requiresApproval :: Maybe Value -> Traversal' Value Bool -> Bool
requiresApproval Nothing _ = False
requiresApproval (Just configValue) path = fromMaybe False $ configValue ^? path

basicInfoFieldRequiresApproval :: Maybe Value -> Key -> Bool
basicInfoFieldRequiresApproval config field =
  requiresApproval config (_basicInfo . key field . _Bool)

serviceAddRequiresApproval :: Maybe Value -> Bool
serviceAddRequiresApproval config =
  requiresApproval config (_service . key "add" . _Bool)

serviceDeleteRequiresApproval :: Maybe Value -> Bool
serviceDeleteRequiresApproval config =
  requiresApproval config (_service . key "delete" . _Bool)

serviceFieldRequiresApproval :: Maybe Value -> Key -> Bool
serviceFieldRequiresApproval config field =
  requiresApproval config (_service . _fields . key field . _Bool)

categoryAddRequiresApproval :: Maybe Value -> Bool
categoryAddRequiresApproval config =
  requiresApproval config (_category . key "add" . _Bool)

categoryDeleteRequiresApproval :: Maybe Value -> Bool
categoryDeleteRequiresApproval config =
  requiresApproval config (_category . key "delete" . _Bool)

categoryFieldRequiresApproval :: Maybe Value -> Key -> Bool
categoryFieldRequiresApproval config field =
  requiresApproval config (_category . _fields . key field . _Bool)

peopleCategoryAddRequiresApproval :: Maybe Value -> Bool
peopleCategoryAddRequiresApproval config =
  requiresApproval config (_peopleCategory . key "add" . _Bool)

peopleCategoryDeleteRequiresApproval :: Maybe Value -> Bool
peopleCategoryDeleteRequiresApproval config =
  requiresApproval config (_peopleCategory . key "delete" . _Bool)

peopleCategoryFieldRequiresApproval :: Maybe Value -> Key -> Bool
peopleCategoryFieldRequiresApproval config field =
  requiresApproval config (_peopleCategory . _fields . key field . _Bool)

_basicInfo :: Traversal' Value Value
_basicInfo = key "basicInfo"

_service :: Traversal' Value Value
_service = key "service"

_category :: Traversal' Value Value
_category = key "category"

_peopleCategory :: Traversal' Value Value
_peopleCategory = key "peopleCategory"

_fields :: Traversal' Value Value
_fields = key "fields"
