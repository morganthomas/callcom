{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module CallCom.Types.Transaction
  ( Transaction (Transaction),
    TransactionPurpose (Creation, Deletion, Issuance, Transfer, Cancellation, ChangePublicKeyOfTo),
    TransactionId (TransactionId),
    TransactionInputs (TransactionInputs),
    TransactionOutputs (TransactionOutputs),
    Signatures (Signatures),
    SignedTransaction (SignedTransaction)
  ) where


import CallCom.Types.Auth (Signature, UserPublicKey)
import CallCom.Types.Positions (Positions)
import CallCom.Types.User (UserId)
import Codec.Serialise (Serialise)
import Data.Aeson (FromJSON)
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Time.Clock (UTCTime)
import Database.PostgreSQL.Simple.FromField (FromField (fromField), fromJSONField)
import Database.PostgreSQL.Simple.ToField (ToField)
import GHC.Generics (Generic)


data SignedTransaction =
  SignedTransaction
    { unsigned :: Transaction,
      signatures :: Signatures
    }
  deriving (Generic, Show)


data Transaction =
  Transaction
    { id :: TransactionId,
      purpose :: TransactionPurpose,
      inputs :: TransactionInputs, -- these positions are destroyed
      outputs :: TransactionOutputs, -- these positions are created
      created :: UTCTime
    }
  deriving (Generic, Show)

instance Serialise Transaction


newtype TransactionId =
  TransactionId
    { unTransactionId :: ByteString
    }
  deriving (Eq, Ord, Generic, Show, Serialise, FromField, ToField)


data TransactionPurpose =
    Creation -- creating new commodities
  | Deletion -- deleting commodities
  | Issuance -- issuing a credit and a debit
  | Transfer -- transferring commodities, credits, and/or debits
  | Cancellation -- deleting a credit and a debit
  | ChangePublicKeyOfTo UserId UserPublicKey -- change spending password pubkey
  deriving (Generic, Show)

instance Serialise TransactionPurpose

instance FromJSON TransactionPurpose

instance FromField TransactionPurpose where
  fromField = fromJSONField


newtype TransactionInputs =
  TransactionInputs
    { unTransactionInputs :: Map UserId Positions
    }
  deriving (Eq, Semigroup, Monoid, Generic, Show, Serialise)


newtype TransactionOutputs =
  TransactionOutputs
    { unTransactionOutputs :: Map UserId Positions
    }
  deriving (Eq, Semigroup, Monoid, Generic, Show, Serialise)


newtype Signatures =
  Signatures
    { unSignatures :: Map UserId Signature
    }
  deriving (Generic, Show)
