{-# LANGUAGE GADTs, KindSignatures, RankNTypes, DeriveFunctor #-}

module FreePlain where

import Prelude hiding (read)
import Data.Function ((&))
import Data.IORef
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

foldFree :: Functor f => (f a -> a) -> (Free f a -> a)
foldFree _ (Pure x) = x
foldFree f (Impure x) = f (fmap (foldFree f) x)

liftFree :: Functor f => f a -> Free f a
liftFree x = Impure (fmap pure x)


data Eff a where
  Input  :: (String -> a) -> Eff a
  Output :: String -> (() -> a) -> Eff a
  Read   :: FilePath -> (String -> a) -> Eff a
  Write  :: FilePath -> String -> (() -> a) -> Eff a
  Random :: R.Random r => (r, r) -> (r -> a) -> Eff a

instance Functor Eff where
  fmap f (Input next)        = Input (f . next)
  fmap f (Output str next)   = Output str (f . next)
  fmap f (Read path next)    = Read path (f . next)
  fmap f (Write path s next) = Write path s (f . next)
  fmap f (Random range next) = Random range (f . next)

type App a = Free Eff a


input :: App String
input = liftFree $ Input id

output :: String -> App ()
output str = liftFree $ Output str id

read :: FilePath -> App String
read path = liftFree $ Read path id

write :: FilePath -> String -> App ()
write path s = liftFree $ Write path s id

random :: R.Random r => (r, r) -> App r
random range = liftFree $ Random range id


runEff :: Eff a -> IO a
runEff (Input next)        = next <$> getLine
runEff (Output str next)   = next <$> putStrLn str
runEff (Read path next)    = next <$> readFile path
runEff (Write path s next) = next <$> writeFile path s
runEff (Random range next) = next <$> randomRIO range

pureEff :: Eff a -> a
pureEff (Input next)           = next "input"
pureEff (Output str next)      = next ()
pureEff (Read path next)       = next "file content"
pureEff (Write path s next)    = next ()
pureEff (Random (min, _) next) = next min


runApp :: App a -> IO a
runApp = runFree runEff

pureApp :: App a -> a
pureApp = foldFree pureEff


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


runProgram :: IO ()
runProgram = runApp program

testProgram :: ()
testProgram = pureApp program
