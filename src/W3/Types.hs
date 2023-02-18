{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module W3.Types where

import Data.Aeson         (FromJSON, ToJSON)
import Data.List          (intercalate)
import Data.Proxy         (Proxy (..))
import Data.Text          ()
import Data.Time.Calendar (Day, fromGregorian)
import GHC.Generics       (Generic)
import Servant.API

type UserApi =
  "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
  :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
  :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email
  -- "users" :> Get '[JSON] [User] :<|> "user" :> "albert" :> Get '[JSON] User :<|> "user" :> "isaac" :> Get '[JSON] User :<|>
  -- :<|> "user" :> Capture "albert" :> Get '[JSON] User
  -- :<|> "user" :> Capture "userid" Integer :> Get '[JSON] User
  -- :<|> "user" :> Capture "userid" Integer :> DeleteNoContent
  -- :<|> "admins" :> Get '[JSON] [User]

type HoistClientApi = Get '[JSON] Int :<|> Capture "n" Int :> Post '[JSON] Int

type W3Api = UserApi :<|> HoistClientApi

data User = User
  { name              :: String
  , age               :: Int
  , email             :: String
  , registration_date :: Day
  } deriving (Eq, Show, Generic)

instance ToJSON User
instance FromJSON User

data SortBy = Age | Name

data Position = Position
  { xCoord :: Int
  , yCoord :: Int
  } deriving (Generic, Show, Eq)

instance ToJSON Position
instance FromJSON Position

newtype HelloMessage = HelloMessage { msg :: String }
  deriving (Generic, Show, Eq)

instance ToJSON HelloMessage
instance FromJSON HelloMessage

data ClientInfo = ClientInfo
  { clientName         :: String
  , clientEmail        :: String
  , clientAge          :: Int
  , clientInterestedIn :: [String]
  } deriving (Generic, Show, Eq)

instance FromJSON ClientInfo
instance ToJSON ClientInfo

data Email = Email
  { from    :: String
  , to      :: String
  , subject :: String
  , body    :: String
  } deriving (Generic, Show, Eq)

instance ToJSON Email
instance FromJSON Email
--
-- | we're going to look things up in DB
--
getPosition :: Int -> Int -> IO Position
getPosition x = pure . Position x

getHello :: Maybe String -> IO HelloMessage
getHello Nothing   = pure . HelloMessage $ "Hello, anonymous coward"
getHello (Just n )=pure . HelloMessage $ ("Hello "  <> n)

doMarketing :: ClientInfo -> IO Email
doMarketing = pure . emailForClient
isaac :: User
isaac = User "Isaac Newton" 372 "isaac@newton.co.uk" (fromGregorian 1683 3 1)

albert :: User
albert = User "Albert Einstein" 136 "ae@mc2.org" (fromGregorian 1905 12 1)

getUsers :: [User]
getUsers = [ isaac, albert]

emailForClient :: ClientInfo -> Email
emailForClient c = Email from' to' subject' body'

  where from'    = "great@company.com"
        to'      = clientEmail c
        subject' = "Hey " ++ clientName c ++ ", we miss you!"
        body'    = "Hi " ++ clientName c ++ ",\n\n"
                ++ "Since you've recently turned " ++ show (clientAge c)
                ++ ", have you checked out our latest "
                ++ intercalate ", " (clientInterestedIn c)
                ++ " products? Give us a visit!"
