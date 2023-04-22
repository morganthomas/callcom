{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module CallCom.Types.TokenBalance
  ( TokenBalance (TokenBalance),
    TokenCreditBalance (TokenCreditBalance),
    TokenDebitBalance (TokenDebitBalance)
  ) where


import Codec.Serialise (Serialise)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import GHC.Generics (Generic)


newtype TokenBalance =
  TokenBalance
    { unTokenBalance :: Integer }
  deriving (Eq, Ord, Generic, Num, Show, Serialise, ToField, FromField)


newtype TokenCreditBalance =
  TokenCreditBalance
    { unTokenCreditBalance :: TokenBalance }
  deriving (Eq, Ord, Generic, Num, Show, Serialise, ToField, FromField)


newtype TokenDebitBalance =
  TokenDebitBalance
    { unTokenDebitBalance :: TokenBalance }
  deriving (Eq, Ord, Generic, Num, Show, Serialise, ToField, FromField)
