{-# LANGUAGE GADTs, KindSignatures, RankNTypes, DeriveFunctor #-}

module Free where

import Prelude hiding (read)
import Data.Function ((&))
import Data.IORef
import Data.Functor.Identity
import Control.Monad (ap, liftM)
import System.Random (randomRIO)
import qualified System.Random as R (Random)


data Free (f :: * -> *) a = Pure a | Impure (f (Free f a))

instance Functor f => Monad (Free f) where
  Pure x   >>= f = f x
  Impure x >>= f = Impure (fmap (>>= f) x)

instance Functor f => Applicative (Free f) where
  pure = Pure
  (<*>) = ap

instance Functor f => Functor (Free f) where
  fmap = liftM

runFree :: Monad m => (forall x. f x -> m x) -> Free f a -> m a
runFree _ (Pure x)   = pure x
runFree f (Impure x) = f x >>= runFree f

foldFree :: (forall x. f x -> x) -> (Free f a -> a)
foldFree f = runIdentity . runFree (Identity . f)

liftFree :: Functor f => f a -> Free f a
liftFree x = Impure (fmap pure x)


data Console a where
  Input  :: (String -> a) -> Console a
  Output :: String -> (() -> a) -> Console a

instance Functor Console where
  fmap f (Input next)      = Input (f . next)
  fmap f (Output str next) = Output str (f . next)

runConsole :: Console a -> IO a
runConsole (Input next)      = next <$> getLine
runConsole (Output str next) = next <$> putStrLn str

pureConsole :: Console a -> a
pureConsole (Input next) = next "mocked input"
pureConsole (Output _ next) = next ()


data FileSystem a where
  Read :: FilePath -> (String -> a) -> FileSystem a

instance Functor FileSystem where
  fmap f (Read path next) = Read path (f . next)

runFileSystem :: FileSystem a -> IO a
runFileSystem (Read path next) = next <$> readFile path

pureFileSystem :: FileSystem a -> a
pureFileSystem (Read _ next) = next "mocked file"


data Random a where
  Random :: R.Random r => (r, r) -> (r -> a) -> Random a

instance Functor Random where
  fmap f (Random range next) = Random range (f . next)

runRandom :: Random a -> IO a
runRandom (Random range next) = next <$> randomRIO range

pureRandom :: Random a -> a
pureRandom (Random (min, _) next) = next min


data Eff a where
  ConsoleEff    :: Console a    -> Eff a
  FileSystemEff :: FileSystem a -> Eff a
  RandomEff     :: Random a     -> Eff a
  deriving Functor

type App a = Free Eff a


input :: App String
input = liftFree $ ConsoleEff (Input id)

output :: String -> App ()
output str = liftFree $ ConsoleEff (Output str id)

read :: FilePath -> App String
read path = liftFree $ FileSystemEff (Read path id)

random :: R.Random r => (r, r) -> App r
random range = liftFree $ RandomEff (Random range id)


runEff :: Eff a -> IO a
runEff (ConsoleEff eff)    = runConsole eff
runEff (FileSystemEff eff) = runFileSystem eff
runEff (RandomEff eff)     = runRandom eff

runPure :: Eff a -> a
runPure (ConsoleEff eff)    = pureConsole eff
runPure (FileSystemEff eff) = pureFileSystem eff
runPure (RandomEff eff)     = pureRandom eff


runApp :: App a -> IO a
runApp = runFree runEff

runTest :: App a -> a
runTest = foldFree runPure


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
testProgram = runTest program
