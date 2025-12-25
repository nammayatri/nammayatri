CREATE TABLE atlas_app.gate_info (
    id varchar(36) PRIMARY KEY,
    point text NOT NULL,
    special_location_id varchar(36) NOT NULL,
    default_driver_extra INT,
    name TEXT NOT NULL,
    geom public.geometry(MultiPolygon),
    address TEXT,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    can_queue_up_on_gate BOOLEAN NOT NULL
);

CREATE INDEX idx_point ON atlas_app.gate_info USING BTREE (point);

ALTER TABLE atlas_app.special_location ADD COLUMN merchant_operating_city_id varchar(36);

-- =========== BELOW FOR LOCAL TO WORK =============
update atlas_app.special_location set merchant_operating_city_id ='f2bc7787-2528-b711-537a-45b059598e5d';

insert into atlas_app.special_location values ('12175690-5356-c62e-ee91-8dfa8c520b39', 'Brigade Road', 'SureBlockedAreaForAutos', '{"GatesInfo { point = LatLong {lat = 12.97522477762719, lon = 77.60736670281528}, name = \"Residency Road Pickup Point\", address = Just \"Near Brigade Road Indian National Flag junction, Shanthala Nagar, Ashok Nagar, Bengaluru, Karnataka 560025, India\" }","GatesInfo { point = LatLong {lat = 12.975117, lon = 77.607718}, name = \"MG Road Pickup Point\", address = Just \"Brigade Road, MG Road Intersection, Haridevpur, Shanthala Nagar, Ashok Nagar, Bengaluru, Karnataka 560001, India\" }"}', public.ST_SetSRID(public.st_geomfromgeojson('{"type":"MultiPolygon","coordinates":[[[[77.607645792,12.975167039],[77.607892961,12.975110568],[77.608027814,12.975071399],[77.608141753,12.975054873],[77.60809549,12.974842833],[77.608095686,12.974721522],[77.608063318,12.974601821],[77.608033228,12.974378971],[77.608003543,12.974182302],[77.607898738,12.974021344],[77.607856276,12.973833327],[77.607812809,12.973681754],[77.607762525,12.973539837],[77.607634991,12.973083434],[77.60757915,12.972789244],[77.607455806,12.972426266],[77.607523863,12.972362675],[77.607546763,12.972237818],[77.607441479,12.972184744],[77.607323892,12.972138509],[77.607149934,12.972052861],[77.606941911,12.971928419],[77.606913248,12.971928888],[77.606685372,12.972026115],[77.60667819,12.972190096],[77.606759193,12.972395552],[77.606875008,12.972664277],[77.606838048,12.972968004],[77.60690253,12.973218462],[77.607027739,12.973470174],[77.607104851,12.973660896],[77.607261414,12.973882962],[77.607322361,12.974043947],[77.607404967,12.974217999],[77.607463452,12.974453726],[77.607501323,12.97457642],[77.607533491,12.974713014],[77.60755117,12.97477481],[77.607562346,12.974845786],[77.607598559,12.974963369],[77.607645792,12.975167039]]]]}'), 0), now(),'d2175690-5356-c62e-ee91-8dfa8c520b39');
insert into atlas_app.gate_info values ('2h8016d0-f9cd-4f9f-886f-bc4cbh6a86e5', 'LatLong {lat = 12.97522477762719, lon = 77.60736670281528}', '12175690-5356-c62e-ee91-8dfa8c520b39', 10, 'Brigade Road Gate', public.ST_SetSRID(public.st_geomfromgeojson('{ "type": "MultiPolygon", "coordinates": [ [ [ [ 77.60701784570114, 12.975357109544092 ], [ 77.60698936115398, 12.975265907157606 ], [ 77.60755498286403, 12.975123155529928 ], [ 77.60757939818961, 12.975214357968767 ], [ 77.60701784570114, 12.975357109544092 ] ] ] ] }'), 0), 'Brigade road gate address', now(), true);
