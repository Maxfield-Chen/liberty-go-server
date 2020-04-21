{-# LANGUAGE AllowAmbiguousTypes       #-}
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

import           Data.Proxy
import qualified Data.Text      as Text
import qualified GameDB         as GDB
import           LGSAPI
import           Reflex
import           Reflex.Dom
import           Servant.API
import           Servant.Reflex
import qualified UserInput


apiClients :: forall t m. (MonadWidget t m) => _
apiClients = client lgsAPI (Proxy :: Proxy m) (Proxy :: Proxy ()) (constDyn (BasePath "/"))

((proposeGame :<|> acceptGameProposal :<|> pass :<|> proposeTerritory :<|> acceptTerritoryProposal :<|> placeStone) :<|> register :<|> login :<|> gamesForUser :<|> getGame) = apiClients
