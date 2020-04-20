{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module LGSAPI where

import qualified Game                                as G
import qualified GameDB                              as GDB
import           Servant.API
import           Servant.Auth.Server
import           Servant.Auth.Server.SetCookieOrphan ()
import qualified UserInput

type Unprotected = "users" :> "register"
                :> ReqBody '[JSON] UserInput.User
                :> Post '[JSON] ()

              :<|> "users" :> "login"
                :> ReqBody '[JSON] UserInput.Login
                :> Verb 'POST 204 '[JSON] (Headers '[ Header "Set-Cookie" SetCookie
                                                    , Header "Set-Cookie" SetCookie]
                                                    NoContent)

              :<|> "users" :> Capture "userId" Int :> "games"
                :> Get '[JSON] [GDB.GameRecord]

              :<|> "play" :> Capture "gameId" Int :> Get '[JSON] (Maybe GDB.GameRecord)


type GameAPI = "play" :> "proposeGame"
                :> ReqBody '[JSON] UserInput.ProposedGame
                :> Post '[JSON] GDB.GameRecord

              :<|> "play" :> Capture "gameId" Int :>
                ("acceptGameProposal" :> ReqBody '[JSON] Bool
                                     :> Post '[JSON] (Maybe G.GameStatus)
                :<|> "pass"
                  :> Put '[JSON] (Maybe G.GameStatus)

                :<|> "proposeTerritory" :> ReqBody '[JSON] G.Territory
                                        :> Put '[JSON] (Maybe G.GameStatus)

                :<|> "acceptTerritoryProposal" :> ReqBody '[JSON] Bool
                                               :> Put '[JSON] (Maybe G.GameStatus)

                :<|> "placeStone" :> ReqBody '[JSON] G.Position
                                  :> Put '[JSON] (Either G.MoveError G.Outcome,G.Game))

type API auths = (Servant.Auth.Server.Auth auths UserInput.User :> GameAPI) :<|> Unprotected
