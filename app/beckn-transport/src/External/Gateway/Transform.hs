{-# LANGUAGE OverloadedLabels #-}

module External.Gateway.Transform where

import App.Types
import Beckn.Types.App
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

mkCatalog :: [ProductInstance] -> Flow Mobility.Catalog
mkCatalog prodInsts = do
  catalogId <- L.generateGUID
  return
    Mobility.Catalog
      { _id = catalogId,
        _categories = [],
        _brands = [],
        _models = [],
        _ttl = Nothing,
        _items = mkItem <$> prodInsts,
        _offers = [],
        _fare_products = []
      }

mkDescriptor :: ProductInstance -> Descriptor
mkDescriptor prodInst =
  Descriptor
    { _name = Nothing,
      _code = Nothing,
      _symbol = Nothing,
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
        _items = [_getProductsId $ pi ^. #_productId],
        _created_at = now,
        _updated_at = now,
        _trip = trip
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
  let bPerson = mkPerson person
   in Driver
        { name = bPerson ^. #name,
          image = bPerson ^. #image,
          dob = bPerson ^. #dob,
          organization_name = bPerson ^. #organization_name,
          gender = bPerson ^. #gender,
          email = bPerson ^. #email,
          phones = bPerson ^. #phones,
          experience = Nothing,
          rating = Nothing
        }

mkPerson :: Person.Person -> BPerson.Person
mkPerson person =
  BPerson.Person
    { name =
        Name
          { _additional_name = person ^. #_middleName,
            _family_name = person ^. #_lastName,
            _given_name = fromMaybe "" (person ^. #_firstName),
            _call_sign = Nothing,
            _honorific_prefix = Nothing,
            _honorific_suffix = Nothing
          },
      image = Nothing,
      dob = Nothing,
      organization_name = Nothing,
      gender = show $ person ^. #_gender,
      email = person ^. #_email,
      phones = maybeToList getPhone
    }
  where
    getPhone =
      case (person ^. #_mobileCountryCode, person ^. #_mobileNumber) of
        (Just ccode, Just number) -> Just $ ccode <> number
        (Nothing, Just number) -> Just number
        _ -> Nothing

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
      _descriptor =
        Descriptor
          { _name = Just $ orgInfo ^. #_name,
            _code = Nothing,
            _symbol = Nothing,
            _short_desc = Nothing,
            _long_desc = Nothing,
            _images = [],
            _audio = Nothing,
            _3d_render = Nothing
          },
      _poc =
        Just $
          BPerson.Person
            { name =
                Name
                  { _additional_name = Nothing,
                    _family_name = Nothing,
                    _given_name = "",
                    _call_sign = Nothing,
                    _honorific_prefix = Nothing,
                    _honorific_suffix = Nothing
                  },
              image = Nothing,
              dob = Nothing,
              organization_name = Just $ orgInfo ^. #_name,
              gender = "",
              email = Nothing,
              phones = maybeToList $ orgInfo ^. #_mobileNumber
            }
    }
