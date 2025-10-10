{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.Management.Volunteer
  ( postVolunteerCreate,
    getVolunteerList,
    postVolunteerUpdate,
  )
where

import qualified API.Types.ProviderPlatform.Management.Volunteer as Common
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Volunteer as DV
import qualified Environment
import Kernel.Prelude
import Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.Volunteer as QVolunteer
import qualified Storage.Queries.VolunteerExtra as QVolunteerExtra
import Tools.Error

---------------------------------------------------------------------
postVolunteerCreate :: ShortId DM.Merchant -> Context.City -> Common.CreateVolunteerReq -> Environment.Flow Common.CreateVolunteerRes
postVolunteerCreate merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)

  now <- getCurrentTime
  volunteerId <- generateGUID

  let volunteer =
        DV.Volunteer
          { id = volunteerId,
            place = req.place,
            vendorId = req.vendorId,
            isActive = req.isActive,
            merchantId = Just merchant.id,
            merchantOperatingCityId = Just merchantOpCityId,
            createdAt = now,
            updatedAt = now
          }

  QVolunteer.create volunteer

  pure $
    Common.CreateVolunteerRes
      { volunteerId = getId volunteerId
      }

---------------------------------------------------------------------
getVolunteerList :: ShortId DM.Merchant -> Context.City -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe Text -> Environment.Flow Common.VolunteerListRes
getVolunteerList merchantShortId opCity mbLimit mbOffset mbVolunteerId mbPlace mbVendorId = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)

  let limit = min maxLimit . fromMaybe defaultLimit $ mbLimit
      offset = fromMaybe 0 mbOffset

  volunteers <- QVolunteerExtra.findAllWithFilters merchantOpCityId limit offset mbVolunteerId mbPlace mbVendorId

  let items = map buildVolunteerListItem volunteers

  pure $
    Common.VolunteerListRes
      { volunteers = items
      }
  where
    maxLimit = 20
    defaultLimit = 10

buildVolunteerListItem :: DV.Volunteer -> Common.VolunteerListItem
buildVolunteerListItem volunteer =
  Common.VolunteerListItem
    { volunteerId = getId volunteer.id,
      place = volunteer.place,
      vendorId = volunteer.vendorId,
      isActive = volunteer.isActive
    }

---------------------------------------------------------------------
postVolunteerUpdate :: ShortId DM.Merchant -> Context.City -> Text -> Common.UpdateVolunteerReq -> Environment.Flow APISuccess
postVolunteerUpdate merchantShortId opCity volunteerIdText req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)

  let volunteerId = Id volunteerIdText
  volunteer <- QVolunteer.findById volunteerId >>= fromMaybeM (InvalidRequest "Volunteer not found")

  -- Verify volunteer belongs to this merchant operating city
  unless (volunteer.merchantOperatingCityId == Just merchantOpCityId) $
    throwError (InvalidRequest "Volunteer does not belong to this merchant operating city")

  now <- getCurrentTime
  let updatedVolunteer =
        volunteer
          { DV.vendorId = req.vendorId `mplus` volunteer.vendorId,
            DV.isActive = req.isActive `mplus` volunteer.isActive,
            DV.updatedAt = now
          }

  QVolunteer.updateByPrimaryKey updatedVolunteer

  pure Success
