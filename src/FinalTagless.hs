module FinalTagless where

import Prelude hiding (read)
import Data.Functor.Identity
import System.Random (Random, randomRIO)


class Monad m => MonadConsole m where
  input :: m String
  output :: String -> m ()

instance MonadConsole IO where
  input = getLine
  output = putStrLn

instance MonadConsole Identity where
  input = pure "console"
  output _ = pure ()


class Monad m => MonadFile m where
  read :: FilePath -> m String

instance MonadFile IO where
  read = readFile

instance MonadFile Identity where
  read _ = pure "content from file"


class Monad m => MonadRandom m where
  random :: Random a => (a, a) -> m a

instance MonadRandom IO where
  random = randomRIO

instance MonadRandom Identity where
  random = pure . fst


class (MonadRandom m, MonadConsole m, MonadFile m) => App m

instance App IO
instance App Identity


quote :: Char -> String -> String
quote c str = [c] <> str <> [c]

quoteAnswer :: Bool -> String -> String
quoteAnswer False answer = quote '"' answer <> " was a horrible answer" 
quoteAnswer True  answer = quote '"' answer <> " was a wonderful answer" 


makeQuestion :: (MonadConsole m, MonadFile m) => m String
makeQuestion = do
  question <- read "question.txt"
  output question
  answer <- input
  if answer == "42"
    then output "correct!"
    else output "incorrect"
  pure answer

program :: App m => m ()
program = do
  answer <- makeQuestion
  nice <- random (False, True)
  output $ quoteAnswer nice answer

run :: IO ()
run = program

test :: Identity ()
test = program
