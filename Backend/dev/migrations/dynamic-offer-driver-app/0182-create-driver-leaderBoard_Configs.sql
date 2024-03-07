-- Don't run in master or prod::
--INSERTING CONFIGS FOR LEADERBOARD
-- ZScoreBase Defined expecting the daily_total_Ride_Distance not greater than 100000000 and weekly_total_Ride_distance  not greater than 1000000000
INSERT INTO atlas_driver_offer_bpp.leader_board_configs (id,leader_board_type,number_of_sets,leader_board_expiry,z_score_base,leader_board_length_limit,merchant_id,is_enabled,merchant_operating_city_id) VALUES
    ('1','DAILY',7,86400,100000000,10,'favorit0-0000-0000-0000-00000favorit',true,'f2bc7787-2528-b711-537a-45b059598e5d'),
    ('2','WEEKLY',4,604800,1000000000,10,'favorit0-0000-0000-0000-00000favorit',true,'f2bc7787-2528-b711-537a-45b059598e5d');
