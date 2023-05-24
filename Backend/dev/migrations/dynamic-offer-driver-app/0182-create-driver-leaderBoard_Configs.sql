-- Creating leaderBoardConfigs
CREATE TABLE atlas_driver_offer_bpp.leader_board_configs (
  id character(36) NOT NULL PRIMARY KEY,
  leader_board_type text NOT NULL,
  number_of_sets INTEGER NOT NULL,
  leader_board_expiry int NOT NULL,
  z_score_base INTEGER NOT NULL,
  leader_board_length_limit INTEGER NOT NULL
);

--INSERTING CONFIGS FOR LEADERBOARD
-- ZScoreBase Defined expecting the daily_total_Ride_Distance not greater than 100000000 and weekly_total_Ride_distance  not greater than 1000000000
INSERT INTO atlas_driver_offer_bpp.leader_board_configs (id,leader_board_type,number_of_sets,leader_board_expiry,z_score_base,leader_board_length_limit) VALUES
    ('1','DAILY',7,86400,100000000,10),
    ('2','WEEKLY',4,604800,1000000000,10);
