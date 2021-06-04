{-#
LANGUAGE TemplateHaskell, GADTs, FlexibleContexts, DataKinds,
         KindSignatures
#-}

module Question where

import Data.Function ((&))
import Data.Functor.Identity
import Control.Monad
import Control.Monad.Freer
import Control.Monad.Freer.TH (makeEffect)

{--
                          sim                       sim
mensagem -> é uma dúvida? -> responde outra pessoa? -> questiona
                | não                  | não
                v                      v                sim
              noop     última mensagem do grupo é dele? -> noop
                                       | não
                                       v
                                   questiona
--}

{-- Flow --}
type Flow a = Either a ()

class Flowable a where
  flow :: Flow a -> a

  toFlow :: a -> Flow a
  toFlow x = Left x

  (?>) :: Bool -> a -> Flow a
  bool ?> value = if bool then Right () else Left value
  infix 3 ?>

instance Flowable Bool where
  flow (Left b) = b
  flow _        = False

instance Flowable (Maybe a) where
  flow (Left x) = x
  flow _        = Nothing

flowExample :: String -> Bool
flowExample msg = flow $ do
  length msg > 0  ?> False
  head msg == 'R' ?> True
  msg == "Renan"  ?> False
{-- Flow --}


data User = User
  { userId   :: Int
  , userName :: String
  } deriving (Show, Eq)

data Message = Message
  { messageId      :: Int
  , messageText    :: String
  , messageAuthor  :: User
  , messageReplyTo :: Maybe User
  } deriving Show

data Group = Group
  { groupId   :: Int
  , groupName :: String
  } deriving Show

type State = [(Group, User)] -- use Map from cointainers


userEx :: User
userEx = User
  { userId = 1
  , userName = "Renan"
  }

messageEx :: Message
messageEx = Message
  { messageId      = 1
  , messageText    = "Hello"
  , messageAuthor  = userEx
  , messageReplyTo = Nothing
  }


data TelegramAPI a where
  SendMessage :: String -> Maybe Message -> TelegramAPI ()

makeEffect ''TelegramAPI

telegramPure :: TelegramAPI a -> a
telegramPure (SendMessage _ _) = ()


class Reference ref where
  newRef   :: Monad m => a -> m (ref a)
  readRef  :: Monad m => ref a -> m a
  writeRef :: Monad m => ref a -> a -> m ()

instance Reference Identity where
  newRef = pure . Identity
  readRef = pure . runIdentity
  writeRef _ _ = pure ()


data Memory a where
  ReadMem  :: Reference ref => ref State -> Memory State
  WriteMem :: Reference ref => ref State -> State -> Memory ()

makeEffect ''Memory

memoryPure :: Memory a -> a
memoryPure (ReadMem _)  = []
memoryPure (WriteMem _ _) = ()


isQuestion :: Message -> Bool
isQuestion = const True

isReplyToOther :: Message -> Bool
isReplyToOther = const False

isNotLastMessage :: State -> Message -> Bool
isNotLastMessage st = const True

shouldAnswer :: State -> Message -> Bool
shouldAnswer st msg = flow $ do
  (not . isQuestion) msg  ?> False
  isReplyToOther msg      ?> True
  isNotLastMessage st msg ?> True


answer :: Member TelegramAPI effs => String -> Message -> Eff effs ()
answer message reply = sendMessage message (Just reply)


type Effects = '[TelegramAPI, Memory]

program :: (Members Effects effs, Reference ref) =>
           ref State -> Message -> Eff effs ()
program stRef message = do
  state <- readMem stRef
  let ans = "Qual é sua dúvida?"
  if shouldAnswer state message
    then answer ans message
    else pure ()


testProgram :: ()
testProgram = program (Identity []) messageEx
  & interpret (pure . telegramPure)
  & interpret (pure . memoryPure)
  & run
