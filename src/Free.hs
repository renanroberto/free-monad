{-# LANGUAGE GADTs, KindSignatures, RankNTypes, DeriveFunctor #-}

module Free where

import Prelude hiding (read)
import Data.Function ((&))
import Data.Functor.Identity
import Control.Monad (ap, liftM)
import System.Random (randomRIO)
import qualified System.Random as R (Random)


data Free (f :: * -> *) a = Pure a | Impure (f (Free f a))

instance Functor f => Monad (Free f) where
  Pure x >>= f   = f x
  Impure x >>= f = Impure (fmap (>>= f) x)

instance Functor f => Applicative (Free f) where
  pure = Pure
  (<*>) = ap

instance Functor f => Functor (Free f) where
  fmap = liftM

runFree :: Monad m => (forall x. f x -> m x) -> Free f a -> m a
runFree _ (Pure x)   = pure x
runFree f (Impure x) = f x >>= runFree f

liftFree :: Functor f => f a -> Free f a
liftFree x = Impure (fmap pure x)

{-- what this should do?
foldFree :: Functor f => (f a -> a) -> (Free f a -> a)
foldFree _ (Pure x) = x
foldFree f (Impure x) = f (fmap (foldFree f) x)
--}


data File a where
  Read :: FilePath -> (String -> a) -> File a

instance Functor File where
  fmap f (Read path next) = Read path (f . next)


data Console a where
  Input  :: (String -> a) -> Console a
  Output :: String -> (() -> a) -> Console a

instance Functor Console where
  fmap f (Input next) = Input (f . next)
  fmap f (Output str next) = Output str (f . next)


data Random a where
  Random :: R.Random r => (r, r) -> (r -> a) -> Random a

instance Functor Random where
  fmap f (Random range next) = Random range (f . next)


data Eff a where
  FileEff    :: File a    -> Eff a
  ConsoleEff :: Console a -> Eff a
  RandomEff  :: Random a  -> Eff a
  deriving Functor

type App a = Free Eff a


read :: FilePath -> App String
read path = liftFree $ FileEff (Read path id)

input :: App String
input = liftFree $ ConsoleEff (Input id)

output :: String -> App ()
output str = liftFree $ ConsoleEff (Output str id)

random :: R.Random r => (r, r) -> App r
random range = liftFree $ RandomEff (Random range id)


runEff :: Eff a -> IO a
runEff (FileEff (Read path next))      = next <$> readFile path
runEff (ConsoleEff (Input next))       = next <$> getLine
runEff (ConsoleEff (Output str next))  = next <$> putStrLn str
runEff (RandomEff (Random range next)) = next <$> randomRIO range

runPure :: Eff a -> Identity a
runPure (FileEff (Read _ next))         = pure $ next "question?"
runPure (ConsoleEff (Input next))       = pure $ next "answer"
runPure (ConsoleEff (Output _ next))    = pure $ next ()
runPure (RandomEff (Random range next)) = pure . next . fst $ range


runApp :: App a -> IO a
runApp = runFree runEff

runTest :: App a -> a
runTest = runIdentity . runFree runPure


quote :: Char -> String -> String
quote c str = [c] <> str <> [c]

quoteAnswer :: Bool -> String -> String
quoteAnswer False answer = quote '"' answer <> " was a horrible answer" 
quoteAnswer True  answer = quote '"' answer <> " was a wonderful answer" 


makeQuestion :: App String
makeQuestion = do
  question <- read "question.txt"
  output question
  answer <- input
  if answer == "42"
    then output "Correct!"
    else output "Incorrect"
  pure answer

program :: App ()
program = do
  answer <- makeQuestion
  nice <- random (False, True)
  output $ quoteAnswer nice answer


run :: IO ()
run = runApp program >> pure ()

test :: ()
test = runTest program
