{-# LANGUAGE QuasiQuotes #-}


module CallCom.DB
  ( migrate,
    getLedgerInception
  ) where


import CallCom.Types.Ledger (LedgerInception)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Database.PostgreSQL.Simple (Query, Connection, Only (fromOnly), execute_, query)
import Database.PostgreSQL.Simple.SqlQQ (sql)


migrate :: MonadIO m => Connection -> m ()
migrate conn = void . liftIO $ execute_ conn migrateQuery

migrateQuery :: Query
migrateQuery =
  [sql|
    CREATE TABLE IF NOT EXISTS ledger(
      ledger_inception TIMESTAMPTZ KEY NOT NULL
    );

    CREATE TABLE IF NOT EXISTS block(
      id BYTEA PRIMARY KEY,
      ledger TIMESTAMPTZ NOT NULL,
      created_time TIMESTAMPTZ NOT NULL,
      parent BYTEA NOT NULL,
      CONSTRAINT fk_ledger
        FOREIGN KEY(ledger)
          REFERENCES ledger(ledger_inception)
    );

    CREATE TABLE IF NOT EXISTS tip(
      ledger TIMPSTAMPTZ PRIMARY KEY,
      tip_block BYTEA NOT NULL,
      CONSTRAINT fk_block
        FOREIGN KEY(tip_block)
          REFERENCES block(id)
    );

    CREATE TABLE IF NOT EXISTS user(
      id BYTEA PRIMARY KEY,
      name TEXT NOT NULL,
      referrer BYTEA,
      created_time TIMESTAMPTZ NOT NULL,
      created_block BYTEA NOT NULL,
      pubkey BYTEA NOT NULL,
      CONSTRAINT fk_created_block
        FOREIGN KEY(created_block)
          REFERENCES block(id)
    );

    CREATE TABLE IF NOT EXISTS commodity_type(
      id BYTEA PRIMARY KEY,
      name TEXT NOT NULL,
      created_time TIMESTAMPTZ NOT NULL,
      created_block BYTEA NOT NULL,
      author BYTEA NOT NULL,
      author_signature BYTEA NOT NULL,
      CONSTRAINT fk_author
        FOREIGN KEY(author)
          REFERENCES user(id)
    );

    CREATE TABLE IF NOT EXISTS token_issue(
      id BYTEA PRIMARY KEY,
      created_time TIMESTAMPTZ NOT NULL,
      created_block BYTEA NOT NULL,
      underlying BYTEA NOT NULL,
      fraction INTEGER NOT NULL,
      CONSTRAINT fk_underlying
        FOREIGN KEY(underlying)
          REFERENCES commodity_type(id)
    );

    CREATE TABLE IF NOT EXISTS token_issuer(
      issuer BYTEA NOT NULL,
      issue BYTEA NOT NULL,
      issuer_signature BYTEA NOT NULL,
      PRIMARY KEY (issuer, issue)
    );

    CREATE TABLE IF NOT EXISTS transaction(
      id BYTEA PRIMARY KEY,
      purpose JSON NOT NULL,
      created TIMESTAMPTZ NOT NULL,
      block BYTEA NOT NULL,
      CONSTRAINT fk_block
        FOREIGN KEY(block)
          REFERENCES block(id)
    );

    CREATE TABLE IF NOT EXISTS transaction_signature(
      transaction BYTEA NOT NULL,
      signer BYTEA NOT NULL,
      signature BYTEA NOT NULL,
      PRIMARY KEY (transaction, signer),
      CONSTRAINT fk_signer
        FOREIGN KEY(signer)
          REFERENCES user(id)
    );

    CREATE TABLE IF NOT EXISTS commodity(
      id BYTEA PRIMARY KEY,
      created_time TIMESTAMPTZ NOT NULL,
      created_transaction BYTEA NOT NULL,
      FOREIGN KEY fk_created_transaction
        FOREIGN KEY(created_transaction)
          REFERENCES transaction(id)
    );

    CREATE TABLE IF NOT EXISTS commodity_description(
      commodity BYTEA NOT NULL,
      key TEXT NOT NULL,
      value TEXT NOT NULL,
      PRIMARY KEY(commodity, key),
      FOREIGN KEY fk_commodity
        FOREIGN KEY(commodity)
          REFERENCES commodity(id)
    );

    CREATE TABLE IF NOT EXISTS commodity_has_type(
      commodity BYTEA NOT NULL,
      commodity_type BYTEA NOT NULL,
      PRIMARY KEY(commodity, commodity_type),
      CONSTRAINT fk_commodity
        FOREIGN KEY(commodity)
          REFERENCES commodity(id),
      CONSTRAINT fk_commodity_type
        FOREIGN KEY(commodity_type)
          REFERENCES commodity_type(id)
    );

    CREATE TABLE IF NOT EXISTS position(
      id BYTEA PRIMARY KEY
    );

    CREATE TABLE IF NOT EXISTS spot_position(
      position BYTEA NOT NULL,
      commodity_type BYTEA NOT NULL,
      commodity BYTEA NOT NULL,
      PRIMARY KEY(position, commodity_type, commodity),
      CONSTRAINT fk_position
        FOREIGN KEY(position)
          REFERENCES position(id),
      CONSTRAINT fk_commodity_type
        FOREIGN KEY(commodity_type)
          REFERENCES commodity_type(id),
      CONSTRAINT fk_commodity
        FOREIGN KEY(commodity)
          REFERENCES commodity(id)
    );

    CREATE TABLE IF NOT EXISTS debit_position(
      position BYTEA NOT NULL,
      issue BYTEA NOT NULL,
      balance INTEGER NOT NULL,
      PRIMARY KEY(position, issue),
      CONSTRAINT fk_position
        FOREIGN KEY(position)
          REFERENCES position(id),
      CONSTRAINT fk_issue
        FOREIGN KEY(issue)
          REFERENCES issue(id)
    );

    CREATE TABLE IF NOT EXISTS credit_position(
      position BYTEA NOT NULL,
      issue BYTEA NOT NULL,
      balance INTEGER NOT NULL,
      PRIMARY KEY(position, issue)
    );

    CREATE TABLE IF NOT EXISTS transaction_input(
      transaction BYTEA NOT NULL,
      owner BYTEA NOT NULL,
      position BYTEA NOT NULL,
      PRIMARY KEY(transaction, owner, position),
      CONSTRAINT fk_transaction
        FOREIGN KEY(transaction)
          REFERENCES transaction(id),
      CONSTRAINT fk_position
        FOREIGN KEY(position)
          REFERENCES position(id)
    );

    CREATE TABLE IF NOT EXISTS transaction_output(
      transaction BYTEA NOT NULL,
      owner BYTEA NOT NULL,
      position BYTEA NOT NULL,
      PRIMARY KEY(transaction, owner, position),
      CONSTRAINT fk_transaction
        FOREIGN KEY(transaction)
          REFERENCES transaction(id),
      CONSTRAINT fk_position
        FOREIGN KEY(position)
          REFERENCES position(id)
    );

    CREATE TABLE IF NOT EXISTS ledger_state(
      id BYTEA PRIMARY KEY
    );

    CREATE TABLE IF NOT EXISTS ledger_state_commodity_type(
      ledger_state BYTEA NOT NULL,
      commodity_type BYTEA NOT NULL,
      PRIMARY KEY(ledger_state, commodity_type),
      CONSTRAINT fk_ledger_state
        FOREIGN KEY(ledger_state)
          REFERENCES ledger_state(id),
      CONSTRAINT fk_commodity_type
        FOREIGN KEY(commodity_type)
          REFERENCES commodity_type(id)
    );

    CREATE TABLE IF NOT EXISTS ledger_state_token_issue(
      ledger_state BYTEA NOT NULL,
      token_issue BYTEA NOT NULL,
      PRIMARY KEY(ledger_state, token_issue),
      CONSTRAINT fk_ledger_state
        FOREIGN KEY(ledger_state)
          REFERENCES ledger_state(id),
      CONSTRAINT fk_token_issue
        FOREIGN KEY(token_issue)
          REFERENCES token_issue(id)
    );

    CREATE TABLE IF NOT EXISTS ledger_state_user(
      ledger_state BYTEA NOT NULL,
      user BYTEA NOT NULL,
      position BYTEA NOT NULL,
      PRIMARY KEY(ledger_state, user),
      CONSTRAINT fk_ledger_state
        FOREIGN KEY(ledger_state)
          REFERENCES ledger_state(id),
      CONSTRAINT fk_user
        FOREIGN KEY(user)
          REFERENCES user(id)
      CONSTRAINT fk_position
        FOREIGN KEY(position)
          REFERENCES position(id)
    );

    CREATE TABLE IF NOT EXISTS ledger_state_transition(
      initial_state BYTEA NOT NULL,
      transaction BYTEA PRIMARY KEY,
      resulting_state BYTEA NOT NULL,
      CONSTRAINT fk_initial_state
        FOREIGN KEY(initial_state)
          REFERENCES ledger_state(id),
      CONSTRAINT fk_transaction
        FOREIGN KEY(transaction)
          REFERENCES transaction(id),
      CONSTRAINT fk_resulting_state
        FOREIGN KEY(resulting_state)
          REFERENCES resulting_state(id)
    );
  |]

getLedgerInception :: MonadIO m => Connection -> m [LedgerInception]
getLedgerInception conn =
  liftIO $ fmap fromOnly <$> query conn getLedgerInceptionQuery ()

getLedgerInceptionQuery :: Query
getLedgerInceptionQuery =
  [sql|
    SELECT ledger_inception FROM ledger;
  |]
