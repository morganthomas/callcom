{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}


module CallCom.Types.HasLedgerStore
  ( HasLedgerStore (getLedgerState, getLedgerTip, getBlock, putBlock),
    getLedger
  ) where


import Control.Lens ((^.))
import CallCom.Types.Ledger (Ledger (EmptyLedger, Ledger), LedgerState, BlockId, Block)
import Data.Generics.Labels ()
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time (UTCTime)


class HasLedgerStore m where
  getLedgerState :: m LedgerState
  getLedgerCreationTime :: m UTCTime
  getLedgerTip :: m (Maybe BlockId)
  getBlock :: BlockId -> m (Maybe Block)
  putBlock :: Block -> m ()


getLedger
  :: ( Monad m, HasLedgerStore m )
  => m Ledger
getLedger = do
  tipm <- getLedgerTip
  t0 <- getLedgerCreationTime
  case tipm of
    Nothing -> pure (EmptyLedger t0)
    Just tip -> Ledger t0 tip <$> getBlocks tip


getBlocks
  :: ( Monad m, HasLedgerStore m )
  => BlockId -> m (Map BlockId Block)
getBlocks tip = do
  bm <- getBlock tip
  case bm of 
    Nothing -> pure mempty
    Just b ->
      (Map.singleton tip b <>)
        <$> maybe (pure mempty) getBlocks
              (b ^. #parent)
