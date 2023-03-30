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
import CallCom.Types.Commodity (CommodityId)
import CallCom.Types.ErrorMessage (ErrorMessage (ErrorMessage))
import CallCom.Types.Ledger (BlockId, Block, Ledger (EmptyLedger, Ledger), LedgerState (LedgerState))
import CallCom.Types.Positions (Positions, subtractPositions)
import CallCom.Types.Transaction (TransactionId, Transaction, SignedTransaction (SignedTransaction), TransactionPurpose (ChangePublicKeyOfTo), TransactionInputs, TransactionOutputs (TransactionOutputs))
import CallCom.Types.User (User (User), UserName (UserName), UserId)
import CallCom.User (getUserId)
import Control.Lens ((^.), (.~), (<&>))
import Control.Monad (forM_, foldM, void)
import Data.Bool (bool)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
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
    mempty
    mempty
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
  pure $
    LedgerState
      (s1 ^. #commodityTypes <> (fst <$> (b ^. #newCommodityTypes)))
      (s1 ^. #tokenIssues <> (fst <$> (b ^. #newTokenIssues)))
      (s1 ^. #users <> (fst <$> (b ^. #newUsers)))
      (s1 ^. #positions)


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
              (s ^. #commodityTypes)
              (s ^. #tokenIssues)
              (Map.insert uid (#pubkey .~ newKey $ u) (s ^. #users))
              (s ^. #positions))
        Nothing ->
          Left . ErrorMessage $
            "tried to change pubkey of a non-existent user: txid "
              <> pack (show (t ^. #id))
    _ ->
      pure
        . LedgerState
            (s ^. #commodityTypes)
            (s ^. #tokenIssues)
            (s ^. #users)
        . createPositions (t ^. #outputs)
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
  verifyNoCommodityTypeIdCollisions s0 b
  verifyCommodityTypeIds b
  verifyCommodityTypeSignatures s0 b
  verifyNoTokenIssueIdCollisions s0 b
  verifyTokenIssueIds b
  verifyTokenIssueSignatures s0 b
  verifyNoUserIdCollisions s0 b
  verifyUserIds b
  verifyResultingUsers s0 b s1
  verifyReferrerSignatures s0 b
  verifyResultingPositions s0 b s1
  verifyPositionsAreNonNegative s1
  verifyTransactionsDoNotConsumeSameInputs b
  verifyTransactions s0 b


-- Verify the block does not try to create a commodity type with the same
-- id as an existing commodity type.
verifyNoCommodityTypeIdCollisions ::
  LedgerState ->
  Block ->
  Either ErrorMessage ()
verifyNoCommodityTypeIdCollisions = todo


-- Verify the block does not try to create a token issue with the same id
-- as an existing token issue.
verifyNoTokenIssueIdCollisions ::
  LedgerState ->
  Block ->
  Either ErrorMessage ()
verifyNoTokenIssueIdCollisions s b =
  bool
    (pure ())
    (Left (ErrorMessage ("token issue id hash collision (most likely a duplicate token issue): " <> pack (show (Map.keys i)))))
    (null i)
  where
    i = Map.intersection (s ^. #tokenIssues) (b ^. #newTokenIssues)


-- Verify the token issue ids are the hashes of the token issues.
verifyTokenIssueIds ::
  Block ->
  Either ErrorMessage ()
verifyTokenIssueIds = todo


-- Verify the commodity type ids are the hashes of the commodity types.
verifyCommodityTypeIds ::
  Block ->
  Either ErrorMessage ()
verifyCommodityTypeIds = todo


-- Verify new commodity type signatures are valid and from the
-- stated creator.
verifyCommodityTypeSignatures ::
  LedgerState ->
  Block ->
  Either ErrorMessage ()
verifyCommodityTypeSignatures = todo


-- Verify new token issue signatures are valid and from all of the
-- stated issuers.
verifyTokenIssueSignatures ::
  LedgerState ->
  Block ->
  Either ErrorMessage ()
verifyTokenIssueSignatures = todo


-- Verify the block does not try to create a user with the same id as an
-- existing user.
verifyNoUserIdCollisions ::
  LedgerState ->
  Block ->
  Either ErrorMessage ()
verifyNoUserIdCollisions s b =
  bool
    (pure ())
    (Left (ErrorMessage ("user id hash collision (most likely a duplicate user): " <> pack (show (Map.keys i)))))
    (null i)
  where
    i = Map.intersection (s ^. #users) (b ^. #newUsers)


-- Verify the new user ids are the hashes of the users.
verifyUserIds ::
  Block ->
  Either ErrorMessage ()
verifyUserIds = todo


-- Verify the resulting state has the correct users in it (the new + the old).
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


-- Verify that each new user comes with the signature of the referrer
-- who is an existing user.
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


-- Verifies that the positions in the resulting state are the correct ones.
verifyResultingPositions ::
  LedgerState ->
  Block ->
  LedgerState ->
  Either ErrorMessage ()
verifyResultingPositions s0 b s1 = do
  s1' <- applyBlockToLedgerState s0 b
  bool
    (pure ())
    (Left . ErrorMessage $
      "resulting positions are not as expected; expected "
        <> pack (show (s1' ^. #positions))
        <> " but got " <> pack (show (s1 ^. #positions)))
    (s1' ^. #positions == s1 ^. #positions)


-- Verifies that all positions in the ledger state are non-negative.
verifyPositionsAreNonNegative ::
  LedgerState ->
  Either ErrorMessage ()
verifyPositionsAreNonNegative s =
  bool
    (pure ())
    (Left . ErrorMessage $ "negative position: "
      <> pack (show s))
    (all (all (>= 0)) (s ^. #positions <&> (^. #debits)) &&
      all (all (>= 0)) (s ^. #positions <&> (^. #credits)))


-- Verifies all conditions required for individual validity of a
-- transaction, for each transaction in a block.
verifyTransactions ::
  LedgerState ->
  Block ->
  Either ErrorMessage ()
verifyTransactions s0 b =
  forM_ (b ^. #transactions) (verifyTransaction s0)


-- Verifies that each spot position (i.e., each commodity)
-- consumed in a block is consumed by a unique transaction in the block.
-- This is checked by checking that we can construct a complete map from
-- commodity id to id of transaction consuming it, without encountering
-- any inconsistencies.
verifyTransactionsDoNotConsumeSameInputs ::
  Block ->
  Either ErrorMessage ()
verifyTransactionsDoNotConsumeSameInputs b =
  void $ foldM addSpotInputsToConsumersMap mempty (b ^. #transactions)


addSpotInputsToConsumersMap ::
  Map CommodityId TransactionId ->
  SignedTransaction ->
  Either ErrorMessage (Map CommodityId TransactionId)
addSpotInputsToConsumersMap m t =
  foldM (addSpotInputToConsumersMap (t ^. #unsigned . #id)) m
    (concat (Map.elems (t ^. #unsigned . #inputs . #unTransactionInputs)
      <&> concatMap (fmap (^. #id) . Set.toList) . Map.elems . (^. #spot)))


addSpotInputToConsumersMap ::
  TransactionId ->
  Map CommodityId TransactionId ->
  CommodityId ->
  Either ErrorMessage (Map CommodityId TransactionId)
addSpotInputToConsumersMap tid m cid =
  maybe
    (pure (Map.insert cid tid m))
    (const (Left (ErrorMessage ("tried to double spend commidity id: " <> pack (show cid)))))
    (Map.lookup cid m)


-- Verifies all conditions required for individual validity of a transaction.
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


-- Check that all signatures are valid.
verifyTransactionSignatures ::
  LedgerState ->
  SignedTransaction ->
  Either ErrorMessage ()
verifyTransactionSignatures s t =
  forM_ (Map.toList (t ^. #signatures . #unSignatures)) $ \(uid, sig) ->
    verifyTransactionSignature s (t ^. #unsigned) uid sig


verifyTransactionSignature ::
  LedgerState ->
  Transaction ->
  UserId ->
  Signature ->
  Either ErrorMessage ()
verifyTransactionSignature s t uid sig = do
  pubkey <- getUserPublicKey s uid
  bool
    (pure ())
    (Left . ErrorMessage $ "signature verification failed: "
      <> pack (show (t, uid, sig)))
    (verifySignature pubkey t sig)


-- Check that all required signatories have provided signatures.
verifyThereAreEnoughSignatures ::
  SignedTransaction ->
  Either ErrorMessage ()
verifyThereAreEnoughSignatures t =
  bool
    (pure ())
    (Left (ErrorMessage ("missing required signatories: " <> pack (show d))))
    (null d)
  where
    d = getRequiredSignatories (t ^. #unsigned)
          `Set.difference` Map.keysSet (t ^. #signatures . #unSignatures)


getRequiredSignatories ::
  Transaction ->
  Set UserId
getRequiredSignatories t =
  case t ^. #purpose of
    ChangePublicKeyOfTo uid _ -> Set.singleton uid
    _ -> getInputAndOutputOwners t


getInputAndOutputOwners ::
  Transaction ->
  Set UserId
getInputAndOutputOwners = todo


getUserPublicKey ::
  LedgerState ->
  UserId ->
  Either ErrorMessage UserPublicKey
getUserPublicKey = curry (fmap (^. #pubkey) . uncurry getUser)


getUser ::
  LedgerState ->
  UserId ->
  Either ErrorMessage User
getUser s uid =
  maybe
    (Left (ErrorMessage "getUser: not found"))
    pure
    (Map.lookup uid (s ^. #users))


-- Verify that there is no spending from a zero balance or
-- spending of a commodity which was not previously declared
-- to exist.
verifyTransactionInputsExist ::
  LedgerState ->
  Transaction ->
  Either ErrorMessage ()
verifyTransactionInputsExist = todo


-- Verify that the transaction conserves value, if it is a transfer.
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
