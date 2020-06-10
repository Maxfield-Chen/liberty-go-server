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
import qualified Data.Map           as M
import           Data.Maybe
import           Data.Text          (Text)
import qualified Data.Text          as T
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Debug.Trace
import           Game               (Position, Space (..), boardPositions,
                                     newGame)
import qualified Game               as G
import           GameDB
import qualified GameLogic          as GL
import           Proofs
import           Reflex
import           Reflex.Dom
import           Servant.Reflex
import qualified ServantClient      as SC
import           Text.Read          (readMaybe)
import           Theory.Named
import qualified UserInput

data Page = Main | Register | Login | Play deriving (Show, Eq)

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
    where styleSheet srcLink = elAttr "link" (M.fromList [("rel", "stylesheet"), ("type","text/css"), ("href", srcLink)]) $ pure ()

bodyEl :: forall t m . MonadWidget t m => m ()
bodyEl = elClass "div" "page-grid" $ do
      curPage       <- headerEl
      evMGameRecord <- getGameEl
      loginB        <- loginEl
      dynGame       <- holdDyn newGame $ maybe newGame _game <$> evMGameRecord
      dynPage       <- holdDyn Main curPage
      boardEv       <- boardEl dynPage dynGame
      posDyn        <- holdDyn (Left "No Pos") $ Right <$> boardEv
      -- TODO: Don't forget to replace this hardcoded gameId
      evOutcome     <- fmapMaybe reqSuccess <$>
        SC.placeStone (constDyn (Right 20)) posDyn (() <$ boardEv)
      pure ()

headerEl :: forall t m. MonadWidget t m => m (Event t Page)
headerEl = elClass "div" "header-el" $ do
  homeBtn     <- elClass "div" "home-button" $ pageButton Main "Go Sensei"
  registerBtn <- elClass "div" "register-button" $ pageButton Register "Register"
  loginBtn    <- elClass "div" "login-button" $ pageButton Login "Login"
  playBtn     <- elClass "div" "play-button" $ pageButton Play "Play"
  pure $ leftmost [homeBtn, registerBtn, loginBtn, playBtn]

boardEl :: forall t m . MonadWidget t m =>
           Dynamic t Page
        -> Dynamic t G.Game
        -> m (Event t Position)
boardEl dynPage dynGame =
    elDynAttr "div" (shouldShow Play "board-grid" <$> dynPage) $ do
      buttonEvs <- foldr (\pos mButtonEvs -> name pos $
                                    \case
                                        Bound boundPos -> do
                                          let dynSpace = (GL.getPosition boundPos) <$> dynGame
                                          buttonEv <- boardButton pos dynSpace
                                          (:) buttonEv <$> mButtonEvs
                                        _ -> error "unbound position when creating boardEl")
                                  (pure [] :: m [Event t Position])
                                  (concat boardPositions)
      pure $ leftmost buttonEvs

shouldShow :: Page -> Text -> Page -> M.Map Text Text
shouldShow componentPage componentClass curPage
  | componentPage == curPage = "class" =: componentClass
  | otherwise = "class" =: "page-hidden"

pageButton :: forall t m. MonadWidget t m =>
              Page
           -> Text
           -> m (Event t Page)
pageButton page btnText = do
  (btn,_) <- elDynAttr' "button" mempty $ text btnText
  pure $ page <$ domEvent Click btn

boardButton :: forall t m. MonadWidget t m =>
                Position
             -> Dynamic t Space
             -> m (Event t Position)
boardButton pos dynSpace = do
                (btn, _) <- elDynAttr' "button" (ffor dynSpace styleSpace) $ text ""
                pure $ pos <$ domEvent Click btn

styleSpace :: Space
           -> M.Map Text Text
styleSpace space = "class" =: (case space of
                                G.Empty -> "space-empty"
                                Black   -> "space-black"
                                White   -> "space-white")

loginEl :: forall t m. MonadWidget t m => m (Event t ())
loginEl = do
  userName :: Dynamic t Text <-
    value <$> inputElement def
  userPassword :: Dynamic t Text <-
    value <$> inputElement def
  b <- button "Login"
  let loginDyn =
        UserInput.Login <$> userName <*> userPassword
  loginEv <- fmapMaybe reqSuccess <$> SC.login (Right <$> loginDyn) b
  pure b

getGameEl :: forall t m. MonadWidget t m => m (Event t (Maybe GameRecord))
getGameEl = do
  rec el "br" blank
      gameId :: Dynamic t (Maybe Int) <-
        fmap (readMaybe . T.unpack) . value <$> textInput def
      b <- button "Retrieve Game"
      wsReq <- inputElement $ def & inputElementConfig_setValue .~ fmap (const "") newMessage
      let newMessage = fmap ((:[]) . encodeUtf8) $ tag (current $ value wsReq) $ keypress Enter wsReq
      wsResp <- do
        ws <- webSocket "ws://localhost:8888" $ def &
          webSocketConfig_send .~ newMessage
        foldDyn (\m ms -> ms ++ [m]) [] $ _webSocket_recv ws
      evFetchGR <- fmapMaybe reqSuccess <$> SC.getGame (Right . fromMaybe (-1) <$> gameId ) b
      gameRecord <- foldDyn (mappend . T.pack . show) "" evFetchGR
      _ <- el "ul"
        $ simpleList wsResp
        $ \msg -> el "li" $ dynText $ fmap decodeUtf8 msg
  pure evFetchGR
