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
import           PageUtil
import           Reflex
import           Reflex.Dom
import           Servant.Reflex
import qualified ServantClient  as SC
import qualified UserInput

registerPage :: forall t m. MonadWidget t m =>
             Dynamic t Page
          -> m (Event t ())
registerPage dynPage = elDynAttr "div" (shouldShow Register "register-page" <$> dynPage) $ do
  text "Email"
  userEmail :: Dynamic t Text <-
    value <$> inputElement def
  text "Username"
  userName :: Dynamic t Text <-
    value <$> inputElement def
  text "Password"
  userPassword :: Dynamic t Text <-
    value <$> inputElement def
  b <- button "Register"
  let userDyn =
        UserInput.User <$> userEmail <*> userName <*> userPassword
  loginEv <- fmapMaybe reqSuccess <$> SC.register (Right <$> userDyn) b
  pure b
