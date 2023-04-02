{-# LANGUAGE QuasiQuotes #-}


module CallCom.DB
  ( migrate
  ) where


import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Database.PostgreSQL.Simple (Query, Connection, execute_)
import Database.PostgreSQL.Simple.SqlQQ (sql)


migrate :: MonadIO m => Connection -> m ()
migrate conn = void . liftIO $ execute_ conn migrateQuery

migrateQuery :: Query
migrateQuery =
  [sql|
    CREATE TABLE IF NOT EXISTS user(
      id BYTEA PRIMARY KEY NOT NULL,
      name TEXT NOT NULL,
      referrer BYTEA,
      created TIMESTAMPTZ NOT NULL,
      pubkey BYTEA NOT NULL,
      CONSTRAINT referrer_id
        KEY(referrer) REFERENCES id
    );
  |]
