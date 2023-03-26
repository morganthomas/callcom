{-# LANGUAGE DeriveGeneric #-}


module CallCom.Types.Ledger
  ( LedgerState (LedgerState),
    Ledger (EmptyLedger, Ledger),
    BlockId (BlockId),
    Block (Block)
  ) where


import CallCom.Types.Positions (Positions)
import CallCom.Types.Transaction (TransactionId, Transaction)
import CallCom.Types.User (UserId, User)
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Time (UTCTime)
import GHC.Generics (Generic)


data LedgerState =
  LedgerState
    { users :: Map UserId User,
      positions :: Map UserId Positions
    }
  deriving Generic


data Ledger =
    EmptyLedger
  | Ledger
    { tip :: BlockId,
      blocks :: Map BlockId Block
    }
  deriving Generic


newtype BlockId =
  BlockId
    { unBlockId :: ByteString
    }
  deriving (Eq, Ord, Generic)


data Block =
  Block
    { newUsers :: Map UserId User,
      created :: UTCTime,
      parent :: Maybe BlockId,
      transactions :: Map TransactionId Transaction
    }
  deriving Generic
