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

import           Data.FileEmbed
import qualified Data.Map        as M
import           LoginPage
import           PageUtil
import           PlayPage
import           ProfilePage
import           ProposeGamePage
import           Reflex
import           Reflex.Dom
import           RegisterPage


devMain :: IO ()
devMain = mainWidgetWithCss css bodyEl
         where css = $(embedFile "./css/lgs.css")

-- TODO: Determine why this returns 403 when finding css files using jsaddle.
main :: IO ()
main = mainWidgetWithHead headEl bodyEl

headEl :: MonadWidget t m => m ()
headEl = do
  el "title" $ text "LGS"
  styleSheet "./css/lgs.css"
    where styleSheet srcLink =
            elAttr "link"
              (M.fromList [("rel", "stylesheet"), ("type","text/css"), ("href", srcLink)]) $
              pure ()

bodyEl :: forall t m . MonadWidget t m => m ()
bodyEl = do
  evHeaderPS   <- headerEl
  elClass "div" "page-grid" $ mdo
    dynPage   <- holdDyn Main $ leftmost [evHeaderPS, Play <$ updated dynGameId]
    loginB    <- loginPage dynPage
    registerB <- registerPage dynPage
    proposeGameB <- proposeGamePage dynPage
    selectedGame <- profilePage dynPage
    dynGameId <- holdDyn (-1) selectedGame
    playPage dynPage dynGameId
    pure ()

headerEl :: forall t m. MonadWidget t m => m (Event t Page)
headerEl = elClass "div" "header-el" $ do
  homeBtn     <- elClass "div" "home-button" $ pageButton Main "Go Sensei"
  profileBtn     <- elClass "div" "profile-button" $ pageButton Profile "Profile"
  registerBtn <- elClass "div" "register-button" $ pageButton Register "Register"
  loginBtn    <- elClass "div" "login-button" $ pageButton Login "Login"
  playBtn     <- elClass "div" "play-button" $ pageButton Play "Play"
  proposeGameBtn     <- elClass "div" "propose-game-button" $ pageButton ProposeGame "Propose Game"
  pure $ leftmost [homeBtn, registerBtn, loginBtn, playBtn, proposeGameBtn, profileBtn]
