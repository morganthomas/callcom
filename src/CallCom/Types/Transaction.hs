{-# LANGUAGE DeriveGeneric #-}


module CallCom.Types.Transaction
  ( Transaction (Transaction),
    TransactionKind (Creation, Transfer, Deletion),
    TransactionId (TransactionId),
    TransactionInputs (TransactionInputs),
    TransactionOutputs (TransactionOutputs),
    Signatures (Signatures)
  ) where


import CallCom.Types.Auth (Signature)
import CallCom.Types.Positions (Positions)
import CallCom.Types.User (UserId)
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)


data Transaction =
  Transaction
    { id :: TransactionId,
      kind :: TransactionKind,
      inputs :: TransactionInputs,
      outputs :: TransactionOutputs,
      created :: UTCTime,
      signature :: Signatures
    }
  deriving Generic


newtype TransactionId =
  TransactionId
    { unTransactionId :: ByteString
    }
  deriving Generic


data TransactionKind =
    Creation
  | Transfer
  | Deletion


newtype TransactionInputs =
  TransactionInputs
    { unTransactionInputs :: Map UserId Positions
    }
  deriving Generic


newtype TransactionOutputs =
  TransactionOutputs
    { unTransactionOutputs :: Map UserId Positions
    }
  deriving Generic

newtype Signatures =
  Signatures
    { unSignatures :: Map UserId Signature
    }
  deriving Generic
