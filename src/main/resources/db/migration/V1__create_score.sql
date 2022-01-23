
CREATE TABLE Score (
  id BIGSERIAL PRIMARY KEY,
  isDeuce BOOLEAN NOT NULL DEFAULT false,
  playerWithAdvantage INT,
  playerThatWon INT,
  p1Score INT NOT NULL DEFAULT 0,
  p2Score INT NOT NULL DEFAULT 0
);

-- INSERT INTO Score DEFAULT VALUES;

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
