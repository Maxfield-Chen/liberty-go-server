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
loginPage dynPage =
  elDynAttr "div" (shouldShow Login "login-page" <$> dynPage) $ divClass "form-prompt" $ mdo
  loginev <- divClass "login-form" $ do
    userName :: Dynamic t Text <- divClass "login-username" $ do
      value <$> placeHolderInput "Username"
    userPassword :: Dynamic t Text <- divClass "login-password" $ do
      value <$> placeHolderInput "Password"
    let loginDyn =
          UserInput.Login <$> userName <*> userPassword
    fmapMaybe reqSuccess <$> SC.login (Right <$> loginDyn) b
  el "br" $ blank
  b <- divClass "submit-button" $ button "Sign in"
  pure b
