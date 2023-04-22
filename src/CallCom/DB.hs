{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}


module CallCom.DB
  ( migrate,
    getLedgerInception,
    getLedgerTip
  ) where


import CallCom.Types.Commodity (Commodity)
import CallCom.Types.CommodityType (CommodityType (CommodityType))
import CallCom.Types.Ledger (BlockId, Block (Block), LedgerInception)
import CallCom.Types.Positions (Positions)
import CallCom.Types.TokenIssue (TokenIssue (TokenIssue))
import CallCom.Types.Transaction (Signatures (Signatures), TransactionInputs (TransactionInputs), TransactionOutputs (TransactionOutputs), SignedTransaction (SignedTransaction), Transaction (Transaction))
import CallCom.Types.User (User (User))
import Control.Monad (void, forM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.ByteString (ByteString)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import qualified Data.Set as Set
import Database.PostgreSQL.Simple (Query, Connection, Only (Only, fromOnly), execute_, query)
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
      referrer_signature BYTEA,
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
      commodity BYTEA NOT NULL,
      PRIMARY KEY(position, commodity),
      CONSTRAINT fk_position
        FOREIGN KEY(position)
          REFERENCES position(id),
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


getLedgerTip :: MonadIO m => Connection -> LedgerInception -> m (Maybe BlockId)
getLedgerTip conn t0 =
  liftIO $ fmap fromOnly . listToMaybe <$> query conn getLedgerTipQuery (Only t0)

getLedgerTipQuery :: Query
getLedgerTipQuery =
  [sql|
    SELECT tip_block FROM tip WHERE ledger = ?
  |]


getBlock :: MonadIO m => Connection -> BlockId -> m (Maybe Block)
getBlock conn bId = runMaybeT $ do
  (bCreated, bParent) <- MaybeT . liftIO $ listToMaybe <$> query conn getBlockQuery (Only bId)
  uRows <- lift . liftIO $ query conn getBlockNewUsersQuery (Only bId)
  let newUsers = Map.fromList
        [ (uid, (User uid nm ref t pk, sig))
          | (uid, nm, ref, sig, t, pk) <- uRows
        ]
  ctRows <- lift . liftIO $ query conn getBlockNewCommodityTypesQuery (Only bId)
  let newCommodityTypes = Map.fromList
        [ (ctId, (CommodityType nm t author, sig))
          | (ctId, nm, t, author, sig) <- ctRows
        ]
  tiRows <- lift . liftIO $ query conn getBlockNewTokenIssuesQuery (Only bId)
  newTokenIssues <- fmap Map.fromList . forM tiRows
    $ \(tiId, t, underlying, frac) -> do
      issuerRows <- lift . liftIO $ query conn getTokenIssuersQuery (Only tiId)
      pure $ (tiId, (TokenIssue underlying frac t (Set.fromList (fst <$> issuerRows)),
                      Signatures (Map.fromList issuerRows)))
  txRows <- lift . liftIO $ query conn getBlockTransactionsQuery (Only bId)
  newTransactions <- fmap Map.fromList . forM txRows
    $ \(txId, purpose, t) -> do
      inRows <- lift . liftIO $ query conn getTxInputsQuery (Only txId)
      outRows <- lift . liftIO $ query conn getTxOutputsQuery (Only txId)
      inputs <- fmap (TransactionInputs . Map.fromList) . forM inRows
        $ \(owner, posId) ->
          (owner,) <$> getPositions conn posId
      outputs <- fmap (TransactionOutputs . Map.fromList) . forM outRows
        $ \(owner, posId) ->
          (owner,) <$> getPositions conn posId
      sigs <- lift . liftIO $ query conn getTxSignaturesQuery (Only txId)
      pure (SignedTransaction (Transaction txId purpose inputs outputs t) (Signatures (Map.fromList sigs)))
  pure (Block newCommodityTypes newTokenIssues newUsers bCreated bParent newTransactions)


newtype PositionId = PositionId { unPositionId :: ByteString }


newtype CommodityId = CommodityId { unCommodityId :: ByteString }


getPositions :: MonadIO m => Connection -> PositionId -> m Positions
getPositions conn posId = do
  spotRows <- lift . liftIO $ query conn getSpotPositionsQuery (Only posId)
  spotPosition <- Set.fromList <$> forM spotRows (getCommodity conn)
  debitPosition <- lift . liftIO
    $ Map.fromList <$> query conn getDebitPositionsQuery (Only posId)
  creditPosition <- lift . liftIO
    $ Map.fromList <$> query conn getCreditPositionsQuery (Only posId)
  pure (Positions spotPosition debitPosition creditPosition)


getCommodity :: MonadIO m => Connection -> CommodityId -> m Commodity
getCommodity = todo


getBlockQuery :: Query
getBlockQuery =
  [sql|
    SELECT created_time, parent WHERE id = ?
  |]

getBlockNewUsersQuery :: Query
getBlockNewUsersQuery =
  [sql|
    SELECT id, name, referrer, referrer_signature, created_time, pubkey
      FROM user WHERE created_block = ?
  |]

getBlockNewCommodityTypesQuery :: Query
getBlockNewCommodityTypesQuery =
  [sql|
    SELECT id, name, created_time, author, author_signature
      FROM commodity_type WHERE created_block = ?
  |]

getBlockNewTokenIssuesQuery :: Query
getBlockNewTokenIssuesQuery =
  [sql|
    SELECT id, created_time, underlying, fraction
      WHERE created_block = ?
  |]

getTokenIssuersQuery :: Query
getTokenIssuersQuery =
  [sql|
    SELECT issuer, issuer_signature WHERE issue = ?
  |]

getBlockTransactionsQuery :: Query
getBlockTransactionsQuery =
  [sql|
    SELECT id, purpose, created FROM transaction WHERE block = ?
  |]

getTxInputsQuery :: Query
getTxInputsQuery =
  [sql|
    SELECT owner, position FROM transaction_input WHERE transaction = ?
  |]

getTxOutputsQuery :: Query
getTxOutputsQuery =
  [sql|
    SELECT owner, position FROM transaction_output WHERE transaction = ?
  |]

getTxSignaturesQuery :: Query
getTxSignaturesQuery =
  [sql|
    SELECT signer, signature FROM transaction_signature WHERE transaction = ?
  |]

getSpotPositionsQuery :: Query
getSpotPositionsQuery =
  [sql|
    SELECT commodity FROM spot_position WHERE 
  |]

getDebitPositionsQuery :: Query
getDebitPositionsQuery =
  [sql|
    SELECT issue, balance FROM debit_position WHERE position = ?
  |]

getCreditPositionsQuery :: Query
getCreditPositionsQuery =
  [sql|
    SELECT issue, balance FROM credit_position WHERE position = ?
  |]


todo :: a
todo = error "todo"
