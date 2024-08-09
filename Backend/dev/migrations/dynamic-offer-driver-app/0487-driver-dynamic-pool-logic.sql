-- ONLY FOR LOCAL
insert into atlas_driver_offer_bpp.app_dynamic_logic (description, domain, logic, name, "order", merchant_operating_city_id)
  values ('Filter unsafe drivers at night', 'POOLING', '{ "filter": [ { "var": "drivers" }, { "!=": [ { "var": "driverPoolResult.driverTags.SafetyCohort" }, "Unsafe" ] } ] }', 'FilterUnsafeAtNight', 0, 'favorit0-0000-0000-0000-00000000city');

insert into atlas_driver_offer_bpp.app_dynamic_logic (description, domain, logic, name, "order", merchant_operating_city_id)
  values ('Calculate safetyScore', 'POOLING', '{ "map": [ { "var": "drivers" }, { "cat": [ { "var": "" }, { "if": [ { "==": [ { "var": "driverPoolResult.driverTags.SafetyCohort" }, "Safe" ] }, { "safetyScore": 0 }, { "if": [ { "==": [ { "var": "driverPoolResult.driverTags.SafetyCohort" }, "Problematic" ] }, { "safetyScore": 0.5 }, { "if": [ { "==": [ { "var": "driverPoolResult.driverTags.SafetyCohort" }, "Unsafe" ] }, { "safetyScore": 1 }, { "safetyScore": 0.25 } ]} ] } ] } ] } ] }', 'SafetyScore', 1, 'favorit0-0000-0000-0000-00000000city');

insert into atlas_driver_offer_bpp.app_dynamic_logic (description, domain, logic, name, "order", merchant_operating_city_id)
  values ('Calculate overall score', 'POOLING', '{ "map": [ { "var": "drivers" }, { "cat": [ { "var": "" }, { "score": { "+": [ { "*": [ { "/": [ { "var": "actualDistanceToPickup" }, 3000 ] }, 0.5 ] }, { "*": [ { "var": "safetyScore" }, 0.5 ] } ] } } ] } ] }', 'OverallScore', 2, 'favorit0-0000-0000-0000-00000000city');

insert into atlas_driver_offer_bpp.app_dynamic_logic (description, domain, logic, name, "order", merchant_operating_city_id)
  values ('Sort Driver on Score', 'POOLING', '{ "sort": [ { "var": "drivers" }, { "var": "score" } ] }', 'Sort', 3, 'favorit0-0000-0000-0000-00000000city');