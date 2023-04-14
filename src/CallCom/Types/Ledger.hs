{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module CallCom.Types.Ledger
  ( LedgerState (LedgerState),
    LedgerInception (LedgerInception),
    Ledger (EmptyLedger, Ledger),
    BlockId (BlockId),
    Block (Block)
  ) where


import CallCom.Types.Auth (Signature)
import CallCom.Types.CommodityType (CommodityTypeId, CommodityType)
import CallCom.Types.Positions (Positions)
import CallCom.Types.TokenIssue (TokenIssueId, TokenIssue)
import CallCom.Types.Transaction (TransactionId, SignedTransaction, Signatures)
import CallCom.Types.User (UserId, User)
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import GHC.Generics (Generic)


data LedgerState =
  LedgerState
    { commodityTypes :: Map CommodityTypeId CommodityType,
      tokenIssues :: Map TokenIssueId TokenIssue,
      users :: Map UserId User,
      positions :: Map UserId Positions
    }
  deriving (Generic, Show)


newtype LedgerInception =
  LedgerInception { unLedgerInception :: UTCTime }
  deriving (Generic, Show, FromField, ToField)


data Ledger =
    EmptyLedger
    { created :: LedgerInception
    }
  | Ledger
    { created :: LedgerInception,
      tip :: BlockId,
      blocks :: Map BlockId Block
    }
  deriving Generic


newtype BlockId =
  BlockId
    { unBlockId :: ByteString
    }
  deriving (Eq, Ord, Generic, Show, ToField, FromField)


data Block =
  Block
    { newCommodityTypes :: Map CommodityTypeId (CommodityType, Signature),
      newTokenIssues :: Map TokenIssueId (TokenIssue, Signatures),
      newUsers :: Map UserId (User, Signature),
      created :: UTCTime,
      parent :: Maybe BlockId,
      transactions :: Map TransactionId SignedTransaction
    }
  deriving (Generic, Show)
