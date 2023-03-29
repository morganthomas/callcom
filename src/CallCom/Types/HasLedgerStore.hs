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


class HasLedgerStore m where
  getLedgerState :: m LedgerState
  getLedgerTip :: m (Maybe BlockId)
  getBlock :: BlockId -> m (Maybe Block)
  putBlock :: Block -> m ()


getLedger
  :: ( Monad m, HasLedgerStore m )
  => m Ledger
getLedger = do
  tipm <- getLedgerTip
  case tipm of
    Nothing -> pure EmptyLedger
    Just tip -> Ledger tip <$> getBlocks tip


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