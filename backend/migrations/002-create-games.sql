CREATE TABLE games
    ( source TEXT NOT NULL
    , id TEXT NOT NULL
    , sgf BLOB NOT NULL
    , updated DATETIME DEFAULT CURRENT_TIMESTAMP NOT NULL
    , PRIMARY KEY (source, id)
    );
