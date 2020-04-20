{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

import           Data.FileEmbed
import qualified Data.Map       as Map
import           Data.Monoid    ((<>))
import           Data.Proxy
import qualified Data.Text      as T
import qualified GameDB         as GDB
import           LGSAPI
import           Reflex
import           Reflex.Dom
import           Servant.API
import           Servant.Reflex
import qualified UserInput

devMain :: IO ()
devMain = mainWidgetWithCss css rgbWidget
         where css = $(embedFile "./css/lgs.css")

-- TODO: Determine why this returns 403 when finding css files using jsaddle.
main :: IO ()
main = mainWidgetWithHead headEl bodyEl

headEl :: MonadWidget t m => m ()
headEl = do
  el "title" $ text "LGS"
  styleSheet "./css/lgs.css"
    where styleSheet link = elAttr "link" (Map.fromList [("rel", "stylesheet"), ("type","text/css"), ("href", link)]) $ pure ()


bodyEl :: MonadWidget t m => m ()
bodyEl = do
  el "h1" $ text "This should be red."
  blank

counterEl :: forall t m. MonadWidget t m => m ()
counterEl = do
  -- let (proposeGame :<|> acceptGameProposal :<|> proposePass :<|> proposeTerritory :<|> acceptTerritoryProposal :<|> placeStone :<|> createNewUser :<|> login :<|> getGamesForPlayer :<|> getGameId) = client lgsAPI (Proxy :: Proxy m) (Proxy :: Proxy []) (constDyn (BasePath "/"))
  let (register :<|> login :<|> gamesForUser :<|> getGame) = client unprotectedAPI (Proxy :: Proxy m) (Proxy :: Proxy ()) (constDyn (BasePath "/"))
  rec el "h2" $ text "How High is the Sky?"
      number <- foldDyn ($) (0 :: Int) $ leftmost [(+ 1) <$ evInc, (+ (-1)) <$ evDec, (const 0) <$ evReset]
      el "div" $ display number
      evInc <- button "higher!"
      evDec <- button "lower!"
      evReset <- button "there is no sky"
      el "br" blank
      ti <- textInput $ def & setValue .~ evText
      evCopy <- button "Write number to TI"
      let evText = tagPromptlyDyn (T.pack . show <$> number) evCopy
  pure ()

rgbWidget :: MonadWidget t m => m ()
rgbWidget = do
  rec el "h2" $ text "Enter color components:"
      el "div" blank
      dfsRed <- labledBox "Red: "
      dfsGreen <- labledBox "Green: "
      dfsBlue <- labledBox "Blue: "
      textArea $
        def & attributes .~ (styleMap <$> value dfsRed <*> value dfsGreen <*> value dfsBlue)
  pure ()

labledBox :: MonadWidget t m => T.Text -> m (TextInput t)
labledBox lbl = el "div" $ do
  text lbl
  textInput $ def & textInputConfig_inputType .~ "number"
                  & textInputConfig_initialValue .~ "0"

styleMap :: T.Text -> T.Text -> T.Text -> Map.Map T.Text T.Text
styleMap r g b = "style" =: mconcat ["background-color: rgb(", r, ", ", g, ", ", b, ")"]
