{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# LANGUAGE MultiParamTypeClasses #-}


module GameDB where

import Database.Beam
import Database.Beam.Sqlite
import Database.SQLite.Simple

import Data.Text (Text)

data UserT f
  = User {_userEmail :: Columnar f Text
         ,_userFirstName :: Columnar f Text
         ,_userLastName :: Columnar f Text
         ,_userPassword :: Columnar f Text} deriving (Generic, Beamable)

type User = UserT Identity
type UserId = PrimaryKey UserT Identity

deriving instance Show User
deriving instance Eq User

instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f Text) deriving (Generic, Beamable)
  primaryKey = UserId . _userEmail

data AddressT f =
  Address
    { _addressId :: Columnar f Int
    , _addressLine1 :: Columnar f Text
    , _addressLine2 :: Columnar f Text
    , _addressForUser :: PrimaryKey UserT f
    }
  deriving (Generic, Beamable)

type Address = AddressT Identity
deriving instance Show (PrimaryKey UserT Identity)
deriving instance Show Address

instance Table AddressT where
  data PrimaryKey AddressT f = AddressId (Columnar f Int)
                               deriving (Generic, Beamable)
  primaryKey = AddressId . _addressId

type AddressId = PrimaryKey AddressT Identity


data ShoppingCartDb f =
  ShoppingCartDb
    { _shoppingCartUsers :: f (TableEntity UserT)
     ,_shoppingCartAddresses :: f (TableEntity AddressT)
    }
  deriving (Generic, Database be)

shoppingCartDb :: DatabaseSettings be ShoppingCartDb
shoppingCartDb =
  defaultDbSettings `withDbModification`
  dbModification
    { _shoppingCartAddresses =
        setEntityName "addresses" <>
        modifyTableFields
          tableModification
            { _addressLine1 = fieldNamed "address1"
            , _addressLine2 = fieldNamed "address2"
            }
    , _shoppingCartUsers = setEntityName "users"
    }

getAllUsersDb =
  let allUsers = all_ (_shoppingCartUsers shoppingCartDb)
   in do conn <- open "./server/shoppingcart1.db"
         runBeamSqliteDebug putStrLn conn $ do
           users <- runSelectReturningList $ select allUsers
           mapM_ (liftIO . print) users
