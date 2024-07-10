-- ADDING BAP EXOPHONE FOR SILIGURI
-- MASTER + PROD
update atlas_app.exophone
  set primary_phone = '03340585574'
  where merchant_operating_city_id = (select id from atlas_app.merchant_operating_city where city = 'Siliguri');