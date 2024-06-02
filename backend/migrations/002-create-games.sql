CREATE TABLE games
    ( source TEXT NOT NULL
    , game_id TEXT NOT NULL
    , owner_id INTEGER
    , sgf BLOB NOT NULL
    , updated DATETIME DEFAULT CURRENT_TIMESTAMP NOT NULL
    , PRIMARY KEY (source, game_id)
    , FOREIGN KEY (owner_id) REFERENCES users(id)
    );

CREATE INDEX idx_games_owner_id ON games(owner_id);
