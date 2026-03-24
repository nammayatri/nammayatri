{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Safety.Common.Handle
  ( SafetyHandle (..),
    SafetyCapabilities (..),
    HasSafetyHandle (..),
    SafetyMonad (..),
    withSafety,
    NotifyContactsReq (..),
    SosPushReq (..),
    RideContextE (..),
    SosTicketReq (..),
  )
where

import Kernel.External.Maps.Types (LatLong)
import Kernel.Prelude hiding (handle)
import Kernel.Types.Id
import Safety.Common.Types
import qualified Safety.Domain.Types.Common as Common
import qualified Safety.Domain.Types.Sos as DSos

-- | Record of callbacks. Shared domain functions depend only on this interface —
-- never on rider-app or driver-app types. Each platform provides its own concrete
-- implementation.
data SafetyHandle m = SafetyHandle
  { -- Builder callbacks (used during SafetyCtx construction)
    fetchPersonDefaults ::
      Id Common.Person ->
      m SafetySettingsPersonDefaults,
    -- rider: reads safety fields off Person record
    -- driver: returns emptyPersonDefaults

    -- Domain callbacks
    findPersonById :: Id Common.Person -> m (Maybe PersonE),
    notifyEmergencyContacts :: SafetyCtx -> NotifyContactsReq -> m (),
    sendSosPushNotification :: SafetyCtx -> SosPushReq -> m (),
    resolveRideContext :: SafetyCtx -> Id Common.Ride -> m (Maybe RideContextE),
    createSosTicket :: SafetyCtx -> SosTicketReq -> m (Maybe Text),
    onSosResolved :: SafetyCtx -> Id DSos.Sos -> m (),
    platformCapabilities :: SafetyCapabilities
  }

data SafetyCapabilities = SafetyCapabilities
  { supportsIvr :: Bool,
    supportsMockDrill :: Bool,
    supportsPoliceCall :: Bool
  }
  deriving (Generic, Show)

class HasSafetyHandle r m where
  getSafetyHandle :: m (SafetyHandle m)

-- | Combines SafetyHandle and SafetyCtx into a single environment record.
-- Eliminates repetitive handle/ctx extraction in every handler.
data SafetyMonad m = SafetyMonad
  { handle :: SafetyHandle m,
    ctx :: SafetyCtx
  }

-- | Entry point for authenticated Safety handlers.
-- Resolves the auth token into SafetyCtx and bundles it with the handle.
--
-- Usage:
-- @
-- postSosMarkRideAsSafe auth sosId req = withSafety auth $ \\env -> do
--   result <- Action.markSosAsSafe sosId env.ctx.personId ...
--   when result.shouldNotifyContacts $
--     notifyEmergencyContacts env.handle env.ctx ...
-- @
withSafety ::
  forall r authToken m a.
  (HasSafetyHandle r m, Monad m) =>
  (SafetyHandle m -> authToken -> m SafetyCtx) ->
  authToken ->
  (SafetyMonad m -> m a) ->
  m a
withSafety buildCtx token action = do
  handle <- getSafetyHandle @r
  ctx <- buildCtx handle token
  action (SafetyMonad handle ctx)

-- | Request types for handle callbacks
data NotifyContactsReq = NotifyContactsReq
  { sosId :: Id DSos.Sos,
    notificationKey :: Text,
    contacts :: Maybe [Text]
  }
  deriving (Generic, Show)

data SosPushReq = SosPushReq
  { sosId :: Id DSos.Sos,
    rideId :: Maybe (Id Common.Ride),
    notificationType :: Text
  }
  deriving (Generic, Show)

data RideContextE = RideContextE
  { rideId :: Id Common.Ride,
    driverName :: Maybe Text,
    vehicleNumber :: Maybe Text,
    rideStartLocation :: Maybe LatLong,
    rideEndLocation :: Maybe LatLong,
    trackingUrl :: Maybe Text
  }
  deriving (Generic, Show)

data SosTicketReq = SosTicketReq
  { sosId :: Id DSos.Sos,
    rideId :: Maybe (Id Common.Ride),
    sosType :: DSos.SosType,
    description :: Maybe Text
  }
  deriving (Generic, Show)
