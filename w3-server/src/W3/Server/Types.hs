{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module W3.Server.Types where

import Control.Monad.IO.Class   (liftIO)
import Data.Proxy               (Proxy (..))
import Data.Text                ()
import Network.Wai.Handler.Warp (defaultSettings, runSettings)
import Servant                  (Application, Handler, Server, serve)
import Servant.API              ((:<|>) ((:<|>)))

import W3.Types                 (ClientInfo, Email, HelloMessage, Position,
                                 W3Api, doMarketing, getHello, getPosition)

server :: Server W3Api
server =
  (position :<|> hello :<|> marketing ) :<|> (getint :<|> postint)

getint :: Handler Int
getint = pure 10
postint :: Int -> Handler Int
postint = pure

w3Api :: Proxy W3Api
w3Api = Proxy

app :: Application
app = serve w3Api server

runApp :: IO ()
runApp = runSettings defaultSettings app

-- | Handler <==> ExceptT ServerError IO
-- newtype ExceptT  e m a = ExceptT (m (Either e a))

position :: Int -> Int -> Handler Position
position x = liftIO . getPosition x

hello :: Maybe String -> Handler HelloMessage
hello = liftIO . getHello

marketing :: ClientInfo -> Handler Email
marketing = liftIO . doMarketing
