
CREATE TABLE Score (
  id BIGSERIAL PRIMARY KEY,
  isDeuce BOOLEAN NOT NULL DEFAULT false,
  playerWithAdvantage INT,
  playerThatWon INT,
  p1Score INT NOT NULL DEFAULT 0,
  p2Score INT NOT NULL DEFAULT 0
);

INSERT INTO Score DEFAULT VALUES;

UPDATE Score SET p1Score = 10 WHERE id = 1;

INSERT INTO Score DEFAULT VALUES;
UPDATE Score SET p1Score = 10, p2Score = 20 WHERE id = 2;

INSERT INTO Score DEFAULT VALUES;
UPDATE Score SET p1Score = 2, playerThatWon = 3 WHERE id = 3;

INSERT INTO Score DEFAULT VALUES;
UPDATE Score SET playerThatWon=2, isDeuce = true WHERE id = 4;

-- CREATE TABLE Score (
--   id SERIAL PRIMARY KEY,
--   isDeuce BOOLEAN,
--   isAdvantage BOOLEAN,
--   playerWithAdvantage INT,
--   isGameComplete BOOLEAN,
--   playerThatWon INT,
--   p1 INT NOT NULL DEFAULT 0,
--   p2 INT NOT NULL DEFAULT 0,
-- );

-- CREATE TABLE Score (
-- id SERIAL PRIMARY KEY,
-- state ENUM('normal', 'deuce', 'p1_advantage', 'p2_advantage', 'p1_win', 'p2_win'),
-- p1 INT NOT NULL DEFAULT 0,
-- p2 INT NOT NULL DEFAULT 0,
-- );
