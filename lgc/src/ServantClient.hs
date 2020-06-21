{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeOperators             #-}

module ServantClient where

import           Data.Map       (Map)
import           Data.Proxy
import           Data.Set       (Set)
import           Data.Text
import           Game           (Game, GameStatus, MoveError, Outcome, Position,
                                 Space)
import qualified GameDB
import           LGSAPI
import qualified OutputTypes    as OT
import           Reflex
import           Reflex.Dom
import           Servant.API
import           Servant.Reflex
import qualified UserInput
import           Web.Cookie


url :: BaseUrl
url = BaseFullUrl Http "localhost" 8888 ""

apiClients :: forall t m. (MonadWidget t m) => _
apiClients = client lgsAPI (Proxy :: Proxy m) (Proxy :: Proxy ()) (constDyn url )

proposeGame :: MonadWidget t m  =>
               Dynamic t (Either Text UserInput.ProposedGame)
               -> Event t ()
               -> m (Event t (ReqResult () OT.GameRecord))

acceptGameProposal :: MonadWidget t m =>
                      Dynamic t (Either Text Int)
                      -> Dynamic t (Either Text Bool)
                      -> Event t ()
                      -> m (Event t (ReqResult () (Maybe GameStatus)))

pass :: MonadWidget t m =>
              Dynamic t (Either Text Int)
           -> Event t ()
           -> m (Event t (ReqResult () (Maybe GameStatus)))

proposeTerritory :: MonadWidget t m =>
                          Dynamic t (Either Text Int)
                          -> Dynamic t (Either Text (Map Space (Set Position)))
                          -> Event t ()
                          -> m (Event t (ReqResult () (Maybe GameStatus)))

acceptTerritoryProposal :: MonadWidget t m =>
                           Dynamic t (Either Text Int)
                         -> Dynamic t (Either Text Bool)
                         -> Event t ()
                         -> m (Event t (ReqResult () (Maybe GameStatus)))

placeStone :: MonadWidget t m =>
                    Dynamic t (Either Text Int)
                    -> Dynamic t (Either Text Position)
                    -> Event t ()
                    -> m (Event t (ReqResult () (Either MoveError Outcome, Game)))

register :: MonadWidget t m =>
                  Dynamic t (Either Text UserInput.RegisterUser)
                  -> Event t ()
                  -> m (Event t (ReqResult () ()))

login :: MonadWidget t m =>
               Dynamic t (Either Text UserInput.Login)
               -> Event t ()
               -> m (Event t (ReqResult () (Headers
                                            '[Header "Set-Cookie" SetCookie,
                                              Header "Set-Cookie" SetCookie] NoContent)))

gamesForUser :: MonadWidget t m =>
                      Dynamic t (Either Text Int)
                      -> Event t ()
                      -> m (Event t (ReqResult () OT.AllGames))

gamesForProfile :: MonadWidget t m =>
                      Event t ()
                      -> m (Event t (ReqResult () OT.AllGames))

userForProfile :: MonadWidget t m =>
                      Event t ()
                      -> m (Event t (ReqResult () OT.User))

getGame :: MonadWidget t m =>
                 Dynamic t (Either Text Int)
                 -> Event t ()
                 -> m (Event t (ReqResult () (Maybe OT.GameRecord)))


((proposeGame :<|> gamesForProfile  :<|> userForProfile :<|> acceptGameProposal :<|> pass :<|> proposeTerritory :<|> acceptTerritoryProposal :<|> placeStone) :<|> (register :<|> login :<|> gamesForUser :<|> getGame) :<|> _) = apiClients
