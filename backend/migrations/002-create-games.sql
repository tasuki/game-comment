CREATE TABLE games
    ( source TEXT NOT NULL
    , game_id TEXT NOT NULL
    , user_id INTEGER
    , sgf BLOB NOT NULL
    , updated DATETIME DEFAULT CURRENT_TIMESTAMP NOT NULL
    , PRIMARY KEY (source, game_id)
    , FOREIGN KEY (user_id) REFERENCES users(id)
    );

CREATE INDEX idx_games_user_id ON games(user_id);
