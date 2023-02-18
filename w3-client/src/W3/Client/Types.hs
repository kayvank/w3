{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module W3.Client.Types where

import Network.HTTP.Client (newManager, defaultManagerSettings)
import Data.Proxy         (Proxy (Proxy))

import Servant.API
import Servant.Client

-- import W3.Server.Types
import W3.Types

userApi :: Proxy UserApi
userApi = Proxy

hoistApi :: Proxy HoistClientApi
hoistApi = Proxy

position
  :<|> hello
  :<|> marketing
  = client userApi

{-
newtype ClientM a = ClientM
  {unClientM :: ReaderT ClinetEnv (ExceptT ClientError IO) a}
-}
userQ :: ClientM (Position, HelloMessage, Email)
userQ = do
  pos <- position 10 10
  message <- hello (Just "servant")
  em <- marketing (ClientInfo "Alp" "alp@foo.com" 26 ["haskell", "mathematics"])
  pure (pos, message, em)

{-
hoistClient
  :: HasClient ClientM api
  => Proxy HoistApi
  -> (forall a. m a -> n a)
  -> Client m HoistApi
  -> Client n HoistApi
-}
-- we're going to hoist into io
-- our conversion function has type: forall a. ClientM a -> IO a
-- the result has type:
-- Client IO HoistClientAPI = IO Int :<|> (Int -> IO Int)
getClients :: ClientEnv -> Client IO HoistClientApi
getClients clientEnv = hoistClient
        hoistApi
        (fmap (either (error . show) id)
        . flip runClientM clientEnv
        )
        (client hoistApi)

getintM :: ClientM Int
postintM :: Int -> ClientM Int
getintM :<|> postintM = client hoistApi

url :: BaseUrl
url = BaseUrl Http "localhost" 3000 ""

run :: IO ()
run = do
  manager' <- newManager defaultManagerSettings
  let env = mkClientEnv manager' (BaseUrl Http "localhost" 3000 "")
  res <- runClientM userQ env
  let (p :<|> p') = getClients env
  case res of
    Left err -> putStrLn $ "Error: " <> show err
    Right (pos, message, em) -> do
      print pos
      print message
      print em
  i <- p
  i' <- p' 10
  print i
  print i'
