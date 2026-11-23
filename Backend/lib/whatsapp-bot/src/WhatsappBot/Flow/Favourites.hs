-- | Favourite places: the rider's saved places (the same list as the Namma
-- Yatri app, @atlas_app.saved_location@) offered at pickup and drop, and
-- the steps that save a new one.
--
-- Not ported from TS: the original connector had no favourites.
--
-- The booking flow decides WHEN to show the list and what picking a place
-- means; this module owns the list layout and the save steps (pin -> name ->
-- replace? -> save). When a save finishes, the booking continues where it left
-- off through the 'Resume' hook the caller passes in, so this module never
-- imports @WhatsappBot.Flow.Booking@ (the edge runs Booking -> Favourites).
--
-- Wire ids (contracts with WhatsApp, do not rename):
--   @fav:pick:<tag>@  a favourite row in the list
--   @fav:share@       "Share new location" row
--   @fav:add@         "Add a favourite" row
--   @fav:save@        "Save place" button on the pickup confirmation
--   @fav:name:Home@ / @fav:name:Work@ / @fav:name:Other@   naming buttons
--   @fav:replace:yes@ / @fav:replace:no@                   replace an existing name
module WhatsappBot.Flow.Favourites
  ( Resume,

    -- * The place list
    favourites,
    sendPlaceList,
    maxListedFavourites,
    findFavourite,
    isSavedPlace,
    savedToPlace,

    -- * Saving a favourite
    startSavePlace,
    startAddFavourite,
    handleFavouritePin,
    handleFavouriteNameTap,
    handleFavouriteText,
    handleReplaceAnswer,
  )
where

import Control.Applicative ((<|>))
import qualified Data.Text as T
import Kernel.Prelude
import WhatsappBot.Env (BotEnv, btn, locationRequest, reply, replyButtons, save)
import qualified WhatsappBot.Env as Fields
import qualified WhatsappBot.Handles as Fields
import WhatsappBot.I18n (LanguageStrings, t)
import qualified WhatsappBot.I18n as Fields
-- Instances only: LanguageStrings fields are dot-accessed via RDP (getField).
import WhatsappBot.I18n.Types ()
import WhatsappBot.Types
import qualified WhatsappBot.Types as BotAddress (BotAddress (..))
import qualified WhatsappBot.Types as BotPlace (BotPlace (..))
import WhatsappBot.Util (isPinPlace, pinPlaceId, placeOrPin)

-- | How the booking continues once a save finishes: a one-line notice
-- ("Saved as Home", or the failure text) to show first, and the context with
-- @state@ set back to where the rider was.
type Resume m = Text -> FlowContext -> m ()

-- ---------------------------------------------------------------------------
-- The place list
-- ---------------------------------------------------------------------------

-- | The rider's favourites (loaded after login by the booking flow's prefetch).
favourites :: FlowContext -> [BotSavedLocation]
favourites ctx = fromMaybe [] ctx.savedLocations

-- | WhatsApp lists hold 10 rows; "Share new location" and "Add a favourite"
-- take two of them.
maxListedFavourites :: Int
maxListedFavourites = 8

-- | Send the pickup/drop list: the favourites, then "Share new location" and
-- "Add a favourite". @label@ is the list button ("Choose pickup").
sendPlaceList :: Monad m => BotEnv m -> Text -> LanguageStrings -> Text -> Text -> [BotSavedLocation] -> m ()
sendPlaceList env to s body label favs =
  void $
    env.sender.sendList
      to
      body
      label
      [ (s.favSectionSaved, map favouriteRow (take maxListedFavourites favs)),
        (s.favSectionOther, [btn s.favShareNew "fav:share", btn s.favAddNew "fav:add"])
      ]

-- | One favourite as a list row: icon + tag as the title, the address below.
-- Title and description are cut to WhatsApp's 24 / 72 characters.
favouriteRow :: BotSavedLocation -> OutButton
favouriteRow loc =
  OutButton
    { btnId = "fav:pick:" <> loc.tag,
      btnTitle = tagIcon loc.tag <> " " <> T.take 20 loc.tag,
      btnDesc =
        case [x | Just x <- [loc.building, loc.area, loc.city], not (T.null x)] of
          [] -> Nothing
          parts -> Just (T.take 72 (T.intercalate ", " parts))
    }

tagIcon :: Text -> Text
tagIcon tag = case T.toLower tag of
  "home" -> "\127968" -- 🏠
  "work" -> "\128188" -- 💼
  _ -> "\11088" -- ⭐

findFavourite :: FlowContext -> Text -> Maybe BotSavedLocation
findFavourite ctx tag = find (\l -> l.tag == tag) (favourites ctx)

-- | Whether this place is already one of the rider's favourites (same point),
-- so the pickup confirmation doesn't offer to save it again.
isSavedPlace :: FlowContext -> BotPlace -> Bool
isSavedPlace ctx p = any (\l -> l.lat == p.lat && l.lon == p.lon) (favourites ctx)

-- | A favourite as a bookable place. Uses the stored address as is, so picking
-- a favourite needs no reverse-geocode call.
savedToPlace :: BotSavedLocation -> BotPlace
savedToPlace loc =
  BotPlace
    { lat = loc.lat,
      lon = loc.lon,
      placeId = fromMaybe (pinPlaceId loc.lat loc.lon) loc.placeId,
      address = BotAddress {area = loc.area, building = loc.building, city = loc.city, country = loc.country, state = loc.state, street = Nothing}
    }

-- | A place to save under @tag@. A pin-only place's placeholder id is not a
-- real place id, so it is not stored ('isPinPlace').
placeToSaved :: Text -> BotPlace -> BotSavedLocation
placeToSaved tag p =
  BotSavedLocation
    { tag = tag,
      lat = p.lat,
      lon = p.lon,
      area = p.address.area,
      building = p.address.building,
      city = p.address.city,
      country = p.address.country,
      state = p.address.state,
      placeId = if isPinPlace p then Nothing else Just p.placeId
    }

-- ---------------------------------------------------------------------------
-- Saving a favourite
-- ---------------------------------------------------------------------------

-- | Start saving @place@ (from "Save place" on the pickup confirmation, or a
-- pin shared after "Add a favourite"): ask for its name. @back@ is the state
-- the booking resumes in afterwards.
startSavePlace :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> BotPlace -> FlowState -> m ()
startSavePlace env ev ctx place back = do
  save env ev ctx {state = SavingFavourite, favouriteDraft = Just place, favouriteName = Nothing, favouriteReturn = Just back}
  askName env ev ctx

-- | "Add a favourite" in the list: ask for the place's pin first.
startAddFavourite :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> m ()
startAddFavourite env ev ctx = do
  let s = t env.cfg.translations ctx.language
  save env ev ctx {state = AwaitingFavouritePin, favouriteDraft = Nothing, favouriteName = Nothing, favouriteReturn = Just ctx.state}
  locationRequest env ev.fromPhone s.favAddPinPrompt

-- | Any message while waiting for the pin to save. A pin moves on to naming;
-- anything else asks for the pin again.
handleFavouritePin :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> m ()
handleFavouritePin env ev ctx = do
  let s = t env.cfg.translations ctx.language
      to = ev.fromPhone
  case (ev.kind, ctx.personId) of
    (_, Nothing) -> sessionLost env ev ctx
    (InLocationPin lat lon mName mAddr, Just pid) -> do
      eplace <- env.backend.reverseGeocode (BotAuth pid) (LatLon lat lon)
      let place0 = placeOrPin lat lon eplace
          place = case mName <|> mAddr of
            Just nm | isNothing place0.address.area -> place0 {BotPlace.address = (place0.address) {BotAddress.area = Just nm}}
            _ -> place0
      startSavePlace env ev ctx place (fromMaybe AwaitingPickup ctx.favouriteReturn)
    _ -> locationRequest env to s.favAddPinPrompt

-- | A naming button: Home and Work save straight away; Other asks the rider
-- to type a name.
handleFavouriteNameTap :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> Resume m -> Text -> m ()
handleFavouriteNameTap env ev ctx resume name
  | name == "Other" = do
    let s = t env.cfg.translations ctx.language
    save env ev ctx {favouriteName = Nothing}
    reply env ev.fromPhone s.favTypeName
  | otherwise = saveAs env ev ctx resume name

-- | Any other message while saving. Typed text is the name; anything else
-- (an old button, a pin) repeats the current question.
handleFavouriteText :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> Resume m -> m ()
handleFavouriteText env ev ctx resume = case ev.kind of
  InText txt
    | name <- T.take 30 (T.strip txt), not (T.null name) -> saveAs env ev ctx {favouriteName = Nothing} resume name
  _ -> case ctx.favouriteName of
    Just name -> askReplace env ev ctx name
    Nothing -> askName env ev ctx

-- | "Replace your Home?" Yes swaps the old place for this one; No asks for a
-- different name.
handleReplaceAnswer :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> Resume m -> Bool -> m ()
handleReplaceAnswer env ev ctx resume yes = do
  let s = t env.cfg.translations ctx.language
  case (ctx.personId, ctx.favouriteDraft, ctx.favouriteName) of
    (Just pid, Just place, Just name)
      | yes -> do
        let saved = placeToSaved name place
        -- One call: the backend keeps the old place when the new one can't be
        -- saved, so on failure the rider's list is unchanged and "couldn't
        -- save" is true.
        erep <- env.backend.replaceSavedLocation (BotAuth pid) saved
        case erep of
          Left _ -> finish env ev ctx resume s.favSaveFailed ctx.savedLocations
          Right () -> finishSaved env ev ctx resume saved
      | otherwise -> do
        save env ev ctx {favouriteName = Nothing}
        reply env ev.fromPhone s.favTypeName
    (Nothing, _, _) -> sessionLost env ev ctx
    _ -> askName env ev ctx

-- | Save the draft under @name@, or ask to replace when the rider already has
-- a favourite with that name (the backend refuses duplicate names). The list
-- is read again first: the one loaded at login misses a place the rider has
-- since added in the app, which would skip the replace question and fail.
saveAs :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> Resume m -> Text -> m ()
saveAs env ev ctx0 resume name = case (ctx0.personId, ctx0.favouriteDraft) of
  (Just pid, Just place) -> do
    let auth = BotAuth pid
    esl <- env.backend.getSavedLocations auth
    let ctx = either (const ctx0) (\locs -> ctx0 {savedLocations = Just locs}) esl
    if isJust (findFavourite ctx name)
      then do
        save env ev ctx {favouriteName = Just name}
        askReplace env ev ctx name
      else addAndFinish env ev ctx resume auth name place
  (Nothing, _) -> sessionLost env ev ctx0
  (_, Nothing) -> finish env ev ctx0 resume (t env.cfg.translations ctx0.language).favSaveFailed ctx0.savedLocations

addAndFinish :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> Resume m -> BotAuth -> Text -> BotPlace -> m ()
addAndFinish env ev ctx resume auth name place = do
  let s = t env.cfg.translations ctx.language
      saved = placeToSaved name place
  eadd <- env.backend.addSavedLocation auth saved
  case eadd of
    Left _ -> finish env ev ctx resume s.favSaveFailed ctx.savedLocations
    Right () -> finishSaved env ev ctx resume saved

-- | A save went through: "Saved as <name>", and the list now holds the new
-- place in place of any old one with that name.
finishSaved :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> Resume m -> BotSavedLocation -> m ()
finishSaved env ev ctx resume saved =
  let s = t env.cfg.translations ctx.language
      others = filter (\l -> l.tag /= saved.tag) (favourites ctx)
   in finish env ev ctx resume (s.favSaved saved.tag) (Just (others <> [saved]))

-- | Clear the save steps, put the rider back where they were, and continue.
finish :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> Resume m -> Text -> Maybe [BotSavedLocation] -> m ()
finish env ev ctx resume notice locs = do
  let ctx' =
        ctx{state = fromMaybe Idle ctx.favouriteReturn,
            savedLocations = locs,
            favouriteDraft = Nothing,
            favouriteName = Nothing,
            favouriteReturn = Nothing
           }
  save env ev ctx'
  resume notice ctx'

askName :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> m ()
askName env ev ctx = do
  let s = t env.cfg.translations ctx.language
  replyButtons env ev.fromPhone s.favNamePrompt [btn s.favNameHome "fav:name:Home", btn s.favNameWork "fav:name:Work", btn s.favNameOther "fav:name:Other"]

askReplace :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> Text -> m ()
askReplace env ev ctx name = do
  let s = t env.cfg.translations ctx.language
  replyButtons env ev.fromPhone (s.favReplacePrompt name) [btn s.favReplaceYes "fav:replace:yes", btn s.favReplaceNo "fav:replace:no"]

sessionLost :: Monad m => BotEnv m -> InboundEvent -> FlowContext -> m ()
sessionLost env ev ctx = do
  let s = t env.cfg.translations ctx.language
  reply env ev.fromPhone s.sessionExpired
  save env ev ctx {state = Idle, favouriteDraft = Nothing, favouriteName = Nothing, favouriteReturn = Nothing}
