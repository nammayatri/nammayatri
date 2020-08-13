{-# LANGUAGE OverloadedLabels #-}

module External.Gateway.Transform where

import App.Types
import Beckn.Types.App
import Beckn.Types.Core.DecimalValue (convertAmountToDecimalValue)
import Beckn.Types.Core.Descriptor
import Beckn.Types.Core.Item
import Beckn.Types.Core.Order (OrderItem (..))
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
import Beckn.Utils.Common (getCurrTime)
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
mkDescriptor _prodInst =
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
  let amt = convertAmountToDecimalValue $ prodInst ^. #_price
   in Price
        { _currency = "INR", -- TODO : Fetch this from product
          _value = Just amt,
          _estimated_value = Just amt,
          _computed_value = Just amt,
          _listed_value = Just amt,
          _offered_value = Just amt,
          _minimum_value = Just amt,
          _maximum_value = Just amt
        }

mkServiceOffer :: Case -> [ProductInstance] -> [ProductInstance] -> Organization -> Flow Mobility.Service
mkServiceOffer c pis _allPis orgInfo = do
  mcatalog <- case pis of
    [] -> return Nothing
    _ -> Just <$> mkCatalog pis
  return
    Mobility.Service
      { _id = _getCaseId $ c ^. #_id,
        _catalog = mcatalog,
        _provider = Just $ mkProvider orgInfo,
        _policies = []
      }

mkOrder :: Case -> ProductInstance -> Maybe Trip -> Flow Mobility.Order
mkOrder _c pri trip = do
  now <- getCurrTime
  return
    Mobility.Order
      { _id = _getProductInstanceId $ pri ^. #_id,
        _items = [OrderItem (_getProductsId $ pri ^. #_productId) Nothing],
        _created_at = now,
        _updated_at = now,
        _state = Nothing,
        _billing = Nothing,
        _payment = Nothing,
        _trip = trip,
        _cancellation_reason_id = Nothing,
        _cancellation_reasons = [],
        _cancellation_policy = Nothing
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
            _long_desc = orgInfo ^. #_info,
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
