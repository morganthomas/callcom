{-# LANGUAGE DeriveGeneric #-}


module CallCom.Types.Transaction
  ( Transaction (Transaction),
    TransactionPurpose (Creation, Transfer, Deletion, ChangePublicKeyOfTo),
    TransactionId (TransactionId),
    TransactionInputs (TransactionInputs),
    TransactionOutputs (TransactionOutputs),
    Signatures (Signatures),
    SignedTransaction (SignedTransaction)
  ) where


import CallCom.Types.Auth (Signature, UserPublicKey)
import CallCom.Types.Positions (Positions)
import CallCom.Types.User (UserId)
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)


data SignedTransaction =
  SignedTransaction
    { unsigned :: Transaction,
      signatures :: Signatures
    }
  deriving Generic


data Transaction =
  Transaction
    { id :: TransactionId,
      purpose :: TransactionPurpose,
      inputs :: TransactionInputs, -- these positions are destroyed
      outputs :: TransactionOutputs, -- these positions are created
      created :: UTCTime
    }
  deriving Generic


newtype TransactionId =
  TransactionId
    { unTransactionId :: ByteString
    }
  deriving (Generic, Show)


data TransactionPurpose =
    Creation
  | Transfer
  | Deletion
  | ChangePublicKeyOfTo UserId UserPublicKey


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
