CREATE TABLE comments
    ( id INTEGER PRIMARY KEY NOT NULL
    , user_id INTEGER NOT NULL
    , source TEXT NOT NULL
    , game_id TEXT NOT NULL
    , move_id INTEGER
    , comment TEXT NOT NULL
    , created DATETIME DEFAULT CURRENT_TIMESTAMP NOT NULL
    , FOREIGN KEY (user_id) REFERENCES users(id)
    , FOREIGN KEY (source, game_id) REFERENCES games(source, game_id)
    );

CREATE INDEX idx_comments_user_id ON comments(user_id);
CREATE INDEX idx_comments_source_game_id ON comments(source, game_id);
CREATE INDEX idx_comments_created ON comments(created);
CREATE UNIQUE INDEX idx_comments_unique_comment ON comments(source, game_id, comment);
