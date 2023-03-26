{-# LANGUAGE DeriveGeneric #-}


module CallCom.Types.Ledger
  ( Ledger (Ledger),
    BlockId (BlockId),
    Block (Block)
  ) where


import CallCom.Types.Positions (Positions)
import CallCom.Types.Transaction (TransactionId, Transaction)
import CallCom.Types.User (UserId, User)
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)


data LedgerState =
  LedgerState
    { users :: Map UserId User,
      positions :: Map UserId Positions
    }
  deriving Generic


data Ledger =
  Ledger
    { tip :: BlockId,
      blocks :: Map BlockId Block
    }
  deriving Generic


newtype BlockId =
  BlockId
    { unBlockId :: Text
    }
  deriving Generic


data Block =
  Block
    { newUsers :: Map UserId User,
      transactions :: Map TransactionId Transaction
    }
  deriving Generic
