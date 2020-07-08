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

module RegisterPage where

import           Data.Text      (Text)
import           Data.Text.Read
import           PageUtil
import           Reflex
import           Reflex.Dom
import           Servant.Reflex
import qualified ServantClient  as SC
import qualified UserInput

registerPage :: forall t m. MonadWidget t m =>
             Dynamic t Page
          -> m (Event t ())
registerPage dynPage = elDynAttr "div" (shouldShow Register "register-page" <$> dynPage) $ divClass "form-prompt" $ do
  userImageValue :: Dynamic t Text <- divClass " register-" $ do
     value <$> placeHolderInput "Profile Image"
  let userImage = (\case
        Left _ -> 0
        Right image -> fst image) . decimal  <$> userImageValue
  userEmail :: Dynamic t Text <- divClass " register-" $ do
    value <$> placeHolderInput "Email"
  userName :: Dynamic t Text <- divClass " register-" $ do
    value <$> placeHolderInput "Username"
  userPassword :: Dynamic t Text <- divClass "register-" $ do
    value <$> placeHolderInput "Password"
  b <- divClass  "submit-button" $ button "Register"
  let userDyn =
        UserInput.RegisterUser <$> userEmail <*> userName <*> userImage <*> userPassword
  loginEv <- fmapMaybe reqSuccess <$> SC.register (Right <$> userDyn) b
  pure b
