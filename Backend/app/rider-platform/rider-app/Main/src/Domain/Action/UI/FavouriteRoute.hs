module Domain.Action.UI.FavouriteRoute
  ( postFavouriteRouteCreate,
    getFavouriteRouteList,
    putFavouriteRoute,
    deleteFavouriteRoute,
    mkFavouriteRouteAPIEntity,
  )
where

import qualified API.Types.UI.FavouriteRoute as API
import qualified BecknV2.FRFS.Enums as Spec
import BecknV2.FRFS.Utils (frfsVehicleCategoryToBecknVehicleCategory)
import qualified Data.Text as T
import qualified Data.Time.Calendar
import qualified Domain.Types.FavouriteRoute as DFavouriteRoute
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import Environment
-- Both preludes export several of the same names; this is the same hiding set
-- Domain.Types.Booking.API uses for the identical two-prelude combination.
import EulerHS.Prelude hiding (elem, find, id, length, map, notElem, null)
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified Storage.Queries.FavouriteRoute as QFavouriteRoute
import qualified Storage.Queries.Person as QP
import Tools.Error

validateFavouriteRoute ::
  DFavouriteRoute.FavouriteRouteType ->
  [DFavouriteRoute.FavouriteNotifyType] ->
  Maybe Text ->
  Maybe TimeOfDay ->
  Maybe TimeOfDay ->
  Maybe Spec.ServiceTierType ->
  [Data.Time.Calendar.DayOfWeek] ->
  Flow ()
validateFavouriteRoute favouriteType notifyOn mbNotifyingStop mbWindowStart mbWindowEnd mbServiceTierType recurrenceRuleDays = do
  when (DFavouriteRoute.APPROACHING `elem` notifyOn && isNothing mbNotifyingStop) $
    throwError (InvalidRequest "notifyingStop is required when notifyOn includes APPROACHING")
  when (favouriteType == DFavouriteRoute.PATTERN) $ do
    -- PATTERN matching always goes through the notifyingStop-scoped query (there is no
    -- route+tier+window match without a stop to key off), so a PATTERN favourite that
    -- never wants APPROACHING (and so never sets notifyingStop) would silently never fire.
    when (DFavouriteRoute.APPROACHING `notElem` notifyOn) $
      throwError (InvalidRequest "notifyOn must include APPROACHING for a PATTERN favourite")
    windowStart <- mbWindowStart & fromMaybeM (InvalidRequest "windowStartTime is required for a PATTERN favourite")
    windowEnd <- mbWindowEnd & fromMaybeM (InvalidRequest "windowEndTime is required for a PATTERN favourite")
    when (windowStart >= windowEnd) $
      throwError (InvalidRequest "windowStartTime must be before windowEndTime; windows crossing midnight are not supported")
    -- A PATTERN favourite fires only for exactly what the rider picked, so the tier and
    -- the days have to be picked. Treating an omission as "any" would push for buses
    -- they never asked about.
    when (isNothing mbServiceTierType) $
      throwError (InvalidRequest "serviceTierType is required for a PATTERN favourite")
    when (null recurrenceRuleDays) $
      throwError (InvalidRequest "recurrenceRuleDays must name at least one day for a PATTERN favourite")

putFavouriteRoute ::
  (Maybe (Id Person.Person), Id Merchant.Merchant) ->
  Id DFavouriteRoute.FavouriteRoute ->
  API.UpdateFavouriteRouteReq ->
  Flow API.FavouriteRouteAPIEntity
putFavouriteRoute (mbPersonId, _merchantId) favouriteId req = do
  personId <- mbPersonId & fromMaybeM (InvalidRequest "Invalid person id")
  favourite <-
    QFavouriteRoute.findByIdAndRider favouriteId personId
      >>= fromMaybeM (FavouriteRouteDoesNotExist favouriteId.getId)

  validateFavouriteRoute favourite.favouriteType req.notifyOn req.notifyingStop req.windowStartTime req.windowEndTime req.serviceTierType req.recurrenceRuleDays

  now <- getCurrentTime
  let updated =
        favourite
          { DFavouriteRoute.notifyOn = req.notifyOn,
            DFavouriteRoute.shouldNotify = req.shouldNotify,
            DFavouriteRoute.recurrenceRuleDays = req.recurrenceRuleDays,
            DFavouriteRoute.notifyingStop = req.notifyingStop,
            DFavouriteRoute.serviceTierType = req.serviceTierType,
            DFavouriteRoute.windowStartTime = req.windowStartTime,
            DFavouriteRoute.windowEndTime = req.windowEndTime,
            DFavouriteRoute.routeShortName = req.routeShortName,
            DFavouriteRoute.fromStopCode = req.fromStopCode,
            DFavouriteRoute.toStopCode = req.toStopCode,
            DFavouriteRoute.status = DFavouriteRoute.ACTIVE,
            DFavouriteRoute.updatedAt = now
          }
  QFavouriteRoute.updateByPrimaryKey updated
  pure $ mkFavouriteRouteAPIEntity updated

postFavouriteRouteCreate ::
  (Maybe (Id Person.Person), Id Merchant.Merchant) ->
  API.CreateFavouriteRouteReq ->
  Flow API.FavouriteRouteAPIEntity
postFavouriteRouteCreate (mbPersonId, merchantId) req = do
  personId <- mbPersonId & fromMaybeM (InvalidRequest "Invalid person id")
  person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)

  when (T.null (T.strip req.routeCode)) $
    throwError (InvalidRequest "routeCode is required")

  let notifyOn = fromMaybe [] req.notifyOn
  validateFavouriteRoute req.favouriteType notifyOn req.notifyingStop req.windowStartTime req.windowEndTime req.serviceTierType (fromMaybe [] req.recurrenceRuleDays)

  mbTripId <- case req.favouriteType of
    DFavouriteRoute.TRIP ->
      Just <$> (req.tripId & fromMaybeM (InvalidRequest "tripId is required for a TRIP favourite"))
    DFavouriteRoute.PATTERN -> pure Nothing

  -- Two favourites are the same subscription only when everything that defines
  -- the journey matches: the route, where the rider boards and alights, the
  -- trigger stop, and -- for PATTERN -- the tier and time window. A rider who
  -- favourites 12M from A to B and then 12M from B to A wants two rows, not one
  -- overwritten, so the stop codes are identity here, not decoration.
  --
  -- `recurrenceRuleDays` is deliberately NOT compared: adding Saturday to a
  -- weekday commute is editing that favourite, not creating a second one that
  -- would then fire alongside it. Genuine edits go through PUT.
  --
  -- The query narrows on (routeCode, notifyingStop), which are set on both modes,
  -- and returns ANY status so an INACTIVE row can be revived. What comes back is
  -- this rider's favourites on this route at this trigger stop -- 0 or 1 in
  -- practice -- so the remainder is compared here, where nullable TimeOfDay and
  -- Text equality is plain Haskell rather than SQL NULL semantics.
  existingRows <- QFavouriteRoute.findAllByRiderRouteAndStop personId req.routeCode req.notifyingStop
  let sameSubscription f =
        f.favouriteType == req.favouriteType
          && f.fromStopCode == req.fromStopCode
          && f.toStopCode == req.toStopCode
          && case req.favouriteType of
            DFavouriteRoute.TRIP -> f.tripId == mbTripId
            DFavouriteRoute.PATTERN ->
              f.serviceTierType == req.serviceTierType
                && f.windowStartTime == req.windowStartTime
                && f.windowEndTime == req.windowEndTime
  now <- getCurrentTime
  case find sameSubscription existingRows of
    Just existing -> do
      let revived =
            existing
              { DFavouriteRoute.status = DFavouriteRoute.ACTIVE,
                DFavouriteRoute.notifyOn = notifyOn,
                DFavouriteRoute.shouldNotify = fromMaybe False req.shouldNotify,
                DFavouriteRoute.recurrenceRuleDays = fromMaybe [] req.recurrenceRuleDays,
                DFavouriteRoute.routeShortName = req.routeShortName,
                DFavouriteRoute.updatedAt = now
              }
      QFavouriteRoute.updateByPrimaryKey revived
      pure $ mkFavouriteRouteAPIEntity revived
    Nothing -> do
      bppConfig <-
        fromMaybeM (InvalidRequest "Integrated BPP config not found") . listToMaybe
          =<< SIBC.findAllIntegratedBPPConfig
            person.merchantOperatingCityId
            (frfsVehicleCategoryToBecknVehicleCategory req.vehicleType)
            DIBC.MULTIMODAL

      newId <- generateGUID
      let favourite =
            DFavouriteRoute.FavouriteRoute
              { id = newId,
                riderId = personId,
                favouriteType = req.favouriteType,
                vehicleType = req.vehicleType,
                routeCode = req.routeCode,
                routeShortName = req.routeShortName,
                tripId = mbTripId,
                notifyOn = notifyOn,
                shouldNotify = fromMaybe True req.shouldNotify,
                notifyingStop = req.notifyingStop,
                serviceTierType = req.serviceTierType,
                windowStartTime = req.windowStartTime,
                windowEndTime = req.windowEndTime,
                recurrenceRuleDays = fromMaybe [] req.recurrenceRuleDays,
                fromStopCode = req.fromStopCode,
                toStopCode = req.toStopCode,
                integratedBppConfigId = bppConfig.id,
                merchantId = merchantId,
                merchantOperatingCityId = person.merchantOperatingCityId,
                status = DFavouriteRoute.ACTIVE,
                createdAt = now,
                updatedAt = now
              }
      QFavouriteRoute.create favourite
      pure $ mkFavouriteRouteAPIEntity favourite

mkFavouriteRouteAPIEntity :: DFavouriteRoute.FavouriteRoute -> API.FavouriteRouteAPIEntity
mkFavouriteRouteAPIEntity f =
  API.FavouriteRouteAPIEntity
    { id = f.id,
      favouriteType = f.favouriteType,
      vehicleType = f.vehicleType,
      routeCode = f.routeCode,
      routeShortName = f.routeShortName,
      tripId = f.tripId,
      notifyOn = f.notifyOn,
      shouldNotify = f.shouldNotify,
      notifyingStop = f.notifyingStop,
      serviceTierType = f.serviceTierType,
      windowStartTime = f.windowStartTime,
      windowEndTime = f.windowEndTime,
      recurrenceRuleDays = f.recurrenceRuleDays,
      fromStopCode = f.fromStopCode,
      toStopCode = f.toStopCode,
      status = f.status,
      createdAt = f.createdAt
    }

getFavouriteRouteList ::
  (Maybe (Id Person.Person), Id Merchant.Merchant) ->
  Flow [API.FavouriteRouteAPIEntity]
getFavouriteRouteList (mbPersonId, _merchantId) = do
  personId <- mbPersonId & fromMaybeM (InvalidRequest "Invalid person id")
  favourites <- QFavouriteRoute.findAllByRider personId DFavouriteRoute.ACTIVE
  pure $ map mkFavouriteRouteAPIEntity (sortOn (Down . (.createdAt)) favourites)

deleteFavouriteRoute ::
  (Maybe (Id Person.Person), Id Merchant.Merchant) ->
  Id DFavouriteRoute.FavouriteRoute ->
  Flow APISuccess
deleteFavouriteRoute (mbPersonId, _merchantId) favouriteId = do
  personId <- mbPersonId & fromMaybeM (InvalidRequest "Invalid person id")
  favourite <-
    QFavouriteRoute.findByIdAndRider favouriteId personId
      >>= fromMaybeM (FavouriteRouteDoesNotExist favouriteId.getId)
  when (favourite.status == DFavouriteRoute.ACTIVE) $
    QFavouriteRoute.updateStatusById DFavouriteRoute.INACTIVE favouriteId
  pure Success
