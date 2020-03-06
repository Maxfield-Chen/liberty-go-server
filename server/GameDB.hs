{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module GameDB where

import Database.Beam
import Database.Beam.Sqlite

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

data ShoppingCartDb f = ShoppingCartDb
                        { _shoppingCartUsers :: f (TableEntity UserT)}
                          deriving (Generic, Database be)

shoppingCartDb :: DatabaseSettings be ShoppingCartDb
shoppingCartDb = defaultDbSettings
