{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}


module CallCom.Ledger
  ( verifyLedger,
    initialLedgerState,
    getResultingLedgerStates,
    verifyBlock
  ) where


import CallCom.Auth (verifySignature)
import CallCom.Types.Auth (UserPublicKey (UserPublicKey), PublicKey (PublicKey), Signature)
import CallCom.Types.ErrorMessage (ErrorMessage (ErrorMessage))
import CallCom.Types.Ledger (BlockId, Block, Ledger (EmptyLedger, Ledger), LedgerState (LedgerState))
import CallCom.Types.Positions (Positions, subtractPositions)
import CallCom.Types.Transaction (Transaction, SignedTransaction (SignedTransaction), TransactionPurpose (ChangePublicKeyOfTo), TransactionInputs, TransactionOutputs (TransactionOutputs))
import CallCom.Types.User (User (User), UserName (UserName), UserId)
import CallCom.User (getUserId)
import Control.Lens ((^.), (.~))
import Control.Monad (forM_, foldM)
import Data.Bool (bool)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (pack)
import Data.Time (UTCTime)


-- Verifies that the given ledger is valid. If it is valid,
-- outputs the resulting ledger state.
verifyLedger :: Ledger -> Either ErrorMessage LedgerState
verifyLedger l = do
  case l of
    EmptyLedger _ -> pure initial
    Ledger _ tip blocks -> do
      maybe (Left (ErrorMessage "tip does not in exist in blocks map"))
        (const (pure ()))
        (Map.lookup tip blocks)
      results <- getResultingLedgerStates initial l
      forM_ (Map.toList blocks) $ \(bId, b) -> do
        s0 <- maybe
                (pure initial)
                (maybe
                  (Left (ErrorMessage
                           ("result of parent of " <> pack (show bId) <>
                             " does not exist in results map")))
                  pure
                  . (`Map.lookup` results))
                (b ^. #parent)
        s1 <- maybe
                (Left (ErrorMessage
                        ("result of " <> pack (show bId) <>
                         "does not exist in results map")))
                pure
                (Map.lookup bId results)
        verifyBlock s0 b s1
      maybe
        (Left (ErrorMessage "result of tip does not exist in results map"))
        pure
        (Map.lookup tip results)
  where
    initial = initialLedgerState (l ^. #created)


initialLedgerState :: UTCTime -> LedgerState
initialLedgerState t0 =
  LedgerState
    (Map.singleton initUserId initUser)
    mempty
  where
    initUserId = getUserId initUserName t0
    initUserName = UserName "morgan.thomas"
    initUser =
      User initUserId
        (UserName "morgan.thomas")
        Nothing
        t0
        (UserPublicKey (PublicKey "r\GS#qT\205i\189\228$\233\129\158\131\227\220\170i~\228[\229N\155\128\191:2\a \208X"))


getResultingLedgerStates ::
  LedgerState ->
  Ledger ->
  Either ErrorMessage (Map BlockId LedgerState)
getResultingLedgerStates initial (Ledger _ tip blocks) =
  getLedgerStatesUnder initial blocks tip
getResultingLedgerStates _ (EmptyLedger _) =
  pure mempty


getLedgerStatesUnder ::
  LedgerState ->
  Map BlockId Block ->
  BlockId ->
  Either ErrorMessage (Map BlockId LedgerState)
getLedgerStatesUnder initial blocks bId = do
  block <-
    maybe
      (Left (ErrorMessage ("block lookup failed: " <> pack (show bId))))
      pure
      (Map.lookup bId blocks)
  case block ^. #parent of
    Nothing -> do
      new <- applyBlockToLedgerState initial block
      pure (Map.singleton bId new)
    Just bId' -> do
      rec <- getLedgerStatesUnder initial blocks bId'
      old <-
        maybe
          (Left (ErrorMessage
                  ("block resulting state lookup failed: "
                    <> pack (show bId'))))
          pure
          (Map.lookup bId' rec)
      new <- applyBlockToLedgerState old block
      pure (Map.insert bId new rec)


applyBlockToLedgerState ::
  LedgerState ->
  Block ->
  Either ErrorMessage LedgerState
applyBlockToLedgerState s0 b = do
  s1 <- foldM applyTransactionToLedgerState s0 (Map.elems (b ^. #transactions))
  pure (LedgerState (s1 ^. #users <> (fst <$> (b ^. #newUsers))) (s1 ^. #positions))


applyTransactionToLedgerState ::
  LedgerState ->
  SignedTransaction ->
  Either ErrorMessage LedgerState
applyTransactionToLedgerState s (SignedTransaction t _) =
  case t ^. #purpose of
    ChangePublicKeyOfTo uid newKey ->
      case Map.lookup uid (s ^. #users) of
        Just u ->
          pure
            (LedgerState
              (Map.insert uid (#pubkey .~ newKey $ u) (s ^. #users))
              (s ^. #positions))
        Nothing ->
          Left . ErrorMessage $
            "tried to change pubkey of a non-existent user: txid "
              <> pack (show (t ^. #id))
    _ ->
      pure . LedgerState (s ^. #users) .
        createPositions (t ^. #outputs)
           $ destroyPositions
               (t ^. #inputs)
               (s ^. #positions)


createPositions ::
  TransactionOutputs ->
  Map UserId Positions ->
  Map UserId Positions
createPositions (TransactionOutputs outs) x =
  x <> outs


destroyPositions ::
  TransactionInputs ->
  Map UserId Positions ->
  Map UserId Positions
destroyPositions =
  Map.differenceWith (curry (pure . uncurry subtractPositions)) . (^. #unTransactionInputs)


-- Verify that the given block is valid relative to the given initial and resulting states.
verifyBlock ::
  LedgerState ->
  Block ->
  LedgerState ->
  Either ErrorMessage ()
verifyBlock s0 b s1 = do
  verifyNoUserIdCollisions s0 b
  verifyResultingUsers s0 b s1
  verifyReferrerSignatures s0 b
  verifyResultingPositions s0 b s1
  verifyPositionsAreNonNegative s1
  verifyTransactionsDoNotConsumeSameInputs b
  verifyTransactions s0 b


verifyNoUserIdCollisions ::
  LedgerState ->
  Block ->
  Either ErrorMessage ()
verifyNoUserIdCollisions s b =
  bool
    (pure ())
    (Left (ErrorMessage ("tried to create an existing user: " <> pack (show (Map.keys i)))))
    (null i)
  where
    i = Map.intersection (s ^. #users) (b ^. #newUsers)


verifyResultingUsers ::
  LedgerState ->
  Block ->
  LedgerState ->
  Either ErrorMessage ()
verifyResultingUsers s0 b s1 =
  bool
    (pure ())
    (Left (ErrorMessage ("incorrect resulting users: " <> pack (show (s0, b, s1)))))
    ((s1 ^. #users) == (s0 ^. #users) `Map.union` (fst <$> (b ^. #newUsers)))


verifyReferrerSignatures ::
  LedgerState ->
  Block ->
  Either ErrorMessage ()
verifyReferrerSignatures st0 b =
  forM_ (Map.elems (b ^. #newUsers))
    (uncurry (verifyReferrerSignature st0))


verifyReferrerSignature ::
  LedgerState ->
  User ->
  Signature ->
  Either ErrorMessage ()
verifyReferrerSignature s0 u sig = do
  referrerId <-
    maybe
      (Left (ErrorMessage ("no referrer: " <> pack (show u))))
      pure
      (u ^. #referrer)
  referrerPubkey <-
    maybe
      (Left (ErrorMessage ("could not find referrer: " <> pack (show (u ^. #referrer)))))
      (pure . (^. #pubkey))
      (Map.lookup referrerId (s0 ^. #users))
  bool
    (pure ())
    (Left (ErrorMessage ("referrer signature check failed: " <> pack (show (u, sig)))))
    (verifySignature referrerPubkey u sig)


verifyResultingPositions ::
  LedgerState ->
  Block ->
  LedgerState ->
  Either ErrorMessage ()
verifyResultingPositions = todo


verifyPositionsAreNonNegative ::
  LedgerState ->
  Either ErrorMessage ()
verifyPositionsAreNonNegative = todo


verifyTransactions ::
  LedgerState ->
  Block ->
  Either ErrorMessage ()
verifyTransactions = todo verifyTransaction


verifyTransactionsDoNotConsumeSameInputs ::
  Block ->
  Either ErrorMessage ()
verifyTransactionsDoNotConsumeSameInputs = todo


verifyTransaction ::
  LedgerState ->
  SignedTransaction ->
  Either ErrorMessage ()
verifyTransaction s0 st@(SignedTransaction t _) = do
  verifyTransactionSignatures s0 st
  verifyThereAreEnoughSignatures st
  verifyTransactionInputsExist s0 t
  verifyTransactionBalances t
  verifyTransactionPurpose t


verifyTransactionSignatures ::
  LedgerState ->
  SignedTransaction ->
  Either ErrorMessage ()
verifyTransactionSignatures = todo


verifyThereAreEnoughSignatures ::
  SignedTransaction ->
  Either ErrorMessage ()
verifyThereAreEnoughSignatures = todo


verifyTransactionInputsExist ::
  LedgerState ->
  Transaction ->
  Either ErrorMessage ()
verifyTransactionInputsExist = todo


verifyTransactionBalances ::
  Transaction  ->
  Either ErrorMessage ()
verifyTransactionBalances = todo


-- Verify that the content of the transaction is
-- consistent with the stated purpose.
verifyTransactionPurpose ::
  Transaction ->
  Either ErrorMessage ()
verifyTransactionPurpose = todo


todo :: a
todo = todo
