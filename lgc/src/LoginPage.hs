{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module LoginPage where

import           Data.Text      (Text)
import           PageUtil
import           Reflex
import           Reflex.Dom
import           Servant.Reflex
import qualified ServantClient  as SC
import qualified UserInput

loginPage :: forall t m. MonadWidget t m =>
             Dynamic t Page
          -> m (Event t ())
loginPage dynPage = elDynAttr "div" (shouldShow Login "login-page" <$> dynPage) $ do
  text "Username"
  userName :: Dynamic t Text <-
    value <$> inputElement def
  text "Password"
  userPassword :: Dynamic t Text <-
    value <$> inputElement def
  b <- button "Login"
  let loginDyn =
        UserInput.Login <$> userName <*> userPassword
  loginEv <- fmapMaybe reqSuccess <$> SC.login (Right <$> loginDyn) b
  pure b
