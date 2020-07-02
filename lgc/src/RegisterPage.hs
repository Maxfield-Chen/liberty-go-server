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
registerPage dynPage = elDynAttr "div" (shouldShow Register "register-page" <$> dynPage) $ divClass "form-prompt" $ do
  userEmail :: Dynamic t Text <- divClass " register-" $ do
    text "Email"
    el "br" blank
    value <$> inputElement def
  el "br" blank
  userName :: Dynamic t Text <- divClass " register-" $ do
    text "Username"
    el "br" blank
    value <$> inputElement def
  el "br" blank
  userPassword :: Dynamic t Text <- divClass "register-" $ do
    text "Password"
    el "br"  blank
    value <$> inputElement def
  el "br" blank
  b <- divClass  "submit-button" $ button "Register"
  let userDyn =
        UserInput.RegisterUser <$> userEmail <*> userName <*> userPassword
  loginEv <- fmapMaybe reqSuccess <$> SC.register (Right <$> userDyn) b
  pure b
