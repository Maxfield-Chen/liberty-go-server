{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExplicitForAll            #-}
{-# LANGUAGE ExplicitNamespaces        #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}


module ServantClient where

import           Data.Proxy
import qualified Data.Text      as Text
import qualified GameDB         as GDB
import           LGSAPI
import           Reflex
import           Reflex.Dom
import           Servant.API
import           Servant.Reflex
import qualified UserInput

-- (proposeGame :<|> acceptGameProposal :<|> proposePass :<|> proposeTerritory :<|> acceptTerritoryProposal :<|> placeStone :<|> createNewUser :<|> login :<|> getGamesForPlayer :<|> getGameId) = client lgsAPI (Proxy @m) (Proxy @()) (constDyn (BasePath "/"))

-- apiClients :: forall t m. (MonadWidget t m) => _
-- apiClients = client lgsAPI (Proxy @m) (Proxy @()) (constDyn (BasePath "/"))

-- proposeGame ::
-- (proposeGame :<|> acceptGameProposal :<|> proposePass :<|> proposeTerritory :<|> acceptTerritoryProposal :<|> placeStone :<|> createNewUser :<|> login :<|> getGamesForPlayer :<|> getGameId) = apiClients
