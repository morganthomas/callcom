{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}


module CallCom.Types.Ledger
  ( LedgerState (LedgerState),
    Ledger (EmptyLedger, Ledger),
    BlockId (BlockId),
    Block (Block)
  ) where


import CallCom.Types.Auth (Signature)
import CallCom.Types.Positions (Positions)
import CallCom.Types.Transaction (TransactionId, SignedTransaction)
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
  deriving (Generic, Show)


data Ledger =
    EmptyLedger
    { created :: UTCTime
    }
  | Ledger
    { created :: UTCTime,
      tip :: BlockId,
      blocks :: Map BlockId Block
    }
  deriving Generic


newtype BlockId =
  BlockId
    { unBlockId :: ByteString
    }
  deriving (Eq, Ord, Generic, Show)


data Block =
  Block
    { newUsers :: Map UserId (User, Signature),
      created :: UTCTime,
      parent :: Maybe BlockId,
      transactions :: Map TransactionId SignedTransaction
    }
  deriving (Generic, Show)
