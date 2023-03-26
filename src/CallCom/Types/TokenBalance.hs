{-# LANGUAGE DeriveGeneric #-}


module CallCom.Types.TokenBalance
  ( TokenBalance (TokenBalance)
  ) where


import GHC.Generics (Generic)


newtype TokenBalance =
  TokenBalance
    { unTokenBalance :: Integer }
  deriving Generic
