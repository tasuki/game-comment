CREATE TABLE games
    ( source TEXT
    , id INTEGER
    , sgf TEXT
    , updated DATETIME DEFAULT CURRENT_TIMESTAMP
    , PRIMARY KEY (source, id)
    );
