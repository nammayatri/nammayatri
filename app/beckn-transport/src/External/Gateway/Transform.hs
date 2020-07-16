{-# LANGUAGE OverloadedLabels #-}

module External.Gateway.Transform where

import App.Types
import Beckn.Types.App
import Beckn.Types.Core.Api
import Beckn.Types.Core.Contact
import Beckn.Types.Core.Descriptor
import Beckn.Types.Core.Item
import Beckn.Types.Core.Person as BPerson
import Beckn.Types.Core.Price
import Beckn.Types.Core.Provider
import qualified Beckn.Types.Core.Tracking as CoreTracking
import Beckn.Types.Mobility.Catalog as Mobility
import Beckn.Types.Mobility.Driver as Mobility
import Beckn.Types.Mobility.Order as Mobility
import Beckn.Types.Mobility.Service as Mobility
import Beckn.Types.Mobility.Trip
import Beckn.Types.Mobility.Vehicle as BVehicle
import Beckn.Types.Storage.Case
import Beckn.Types.Storage.Organization as Organization
import Beckn.Types.Storage.Person as Person
import Beckn.Types.Storage.ProductInstance as ProductInstance
import Beckn.Types.Storage.Vehicle as Vehicle
import Beckn.Utils.Extra (getCurrentTimeUTC)
import Data.Text as T
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified Utils.Defaults as Defaults

mkCatalog :: [ProductInstance] -> Flow Mobility.Catalog
mkCatalog prodInsts = do
  catalogId <- L.generateGUID
  return
    Mobility.Catalog
      { _id = Just catalogId,
        _categories = [],
        _brands = [],
        _exp = Nothing,
        _items = mkItem <$> prodInsts,
        _fare_products = []
      }

mkDescriptor :: ProductInstance -> Descriptor
mkDescriptor prodInst =
  Descriptor
    { _id = _getProductInstanceId $ prodInst ^. #_id,
      _name = Nothing,
      _code = Nothing,
      _sym = Nothing,
      _short_desc = Nothing,
      _long_desc = Nothing,
      _images = [],
      _audio = Nothing,
      _3d_render = Nothing
    }

mkItem :: ProductInstance -> Item
mkItem prodInst =
  Item
    { _id = _getProductInstanceId $ prodInst ^. #_id,
      _parent_item_id = Nothing,
      _descriptor = mkDescriptor prodInst,
      _price = mkPrice prodInst,
      _promotional = False,
      _category_id = Nothing,
      _model_id = Nothing,
      _brand_id = Nothing,
      _tags = [],
      _ttl = Nothing
    }

mkPrice :: ProductInstance -> Price
mkPrice prodInst =
  Price
    { _currency = "INR", -- TODO : Fetch this from product
      _estimated_value = prodInst ^. #_price,
      _computed_value = prodInst ^. #_price,
      _listed_value = prodInst ^. #_price,
      _offered_value = prodInst ^. #_price,
      _range = Nothing,
      _breakup = []
    }

mkServiceOffer :: Case -> [ProductInstance] -> [ProductInstance] -> Maybe Organization -> Flow Mobility.Service
mkServiceOffer c pis allPis orgInfo = do
  catalog <- mkCatalog pis
  return
    Mobility.Service
      { _id = _getCaseId $ c ^. #_id,
        _catalog = Just catalog,
        _provider = mkProvider <$> orgInfo,
        _policies = []
      }

mkOrder :: Case -> ProductInstance -> Maybe Trip -> Flow Mobility.Order
mkOrder c pi trip = do
  now <- getCurrentTimeUTC
  return
    Mobility.Order
      { _id = _getProductInstanceId $ pi ^. #_id,
        _state = Nothing,
        _billing = Nothing,
        _fulfillment = Nothing,
        _created_at = now,
        _updated_at = now,
        _trip = trip,
        _invoice = Nothing
      }

baseTrackingUrl :: Text
baseTrackingUrl = "http://api.sandbox.beckn.juspay.in/transport/v1/location"

mkTracking :: Text -> Text -> CoreTracking.Tracking
mkTracking method dataUrl =
  CoreTracking.Tracking
    { _url = if method == "PULL" then Just dataUrl else Nothing,
      _required_params = Nothing,
      _metadata = Nothing
    }

mkDriverObj :: Person.Person -> Driver
mkDriverObj person =
  Driver
    { descriptor = mkPerson person,
      experience = Nothing
    }

mkPerson :: Person.Person -> BPerson.Person
mkPerson person =
  BPerson.Person
    { title = "",
      first_name = fromMaybe "" (person ^. #_firstName),
      middle_name = fromMaybe "" (person ^. #_middleName),
      last_name = fromMaybe "" (person ^. #_lastName),
      full_name = fromMaybe "" (person ^. #_fullName),
      image = Nothing,
      dob = Nothing,
      gender = show $ person ^. #_gender,
      contact =
        Contact
          { email = person ^. #_email,
            mobile =
              Just
                Mobile
                  { country_code = person ^. #_mobileCountryCode,
                    number = person ^. #_mobileNumber -- TODO: need to take last 10
                  },
            landline = Nothing,
            ivr = []
          }
    }

mkVehicleObj :: Vehicle.Vehicle -> BVehicle.Vehicle
mkVehicleObj vehicle =
  BVehicle.Vehicle
    { category = show <$> vehicle ^. #_category,
      capacity = vehicle ^. #_capacity,
      make = vehicle ^. #_make,
      model = vehicle ^. #_model,
      size = vehicle ^. #_size,
      variant = maybe "" show (vehicle ^. #_variant),
      color = vehicle ^. #_color,
      energy_type = show <$> vehicle ^. #_energyType,
      registration =
        Just
          Registration
            { category = maybe "COMMERCIAL" show (vehicle ^. #_registrationCategory),
              number = vehicle ^. #_registrationNo
            }
    }

mkProvider :: Organization -> Provider
mkProvider orgInfo =
  Provider
    { _id = _getOrganizationId $ orgInfo ^. #_id,
      _name = orgInfo ^. #_name,
      _website = fromMaybe "" (orgInfo ^. #_callbackUrl),
      _contact = mkContact (orgInfo ^. #_mobileNumber),
      _api = Api "" Defaults.localTime
    }

mkContact :: Maybe Text -> Contact
mkContact mobileNumber =
  Contact
    { email = Nothing,
      mobile = Just (Mobile Nothing mobileNumber),
      landline = Nothing,
      ivr = []
    }
