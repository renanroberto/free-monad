module FinalTagless where

import Prelude hiding (read)
import Data.Functor.Identity
import System.Random (Random, randomRIO)


class Monad m => HasConsole m where
  input :: m String
  output :: String -> m ()

instance HasConsole IO where
  input = getLine
  output = putStrLn

instance HasConsole Identity where
  input = pure "console"
  output _ = pure ()


class Monad m => HasFile m where
  read :: FilePath -> m String

instance HasFile IO where
  read = readFile

instance HasFile Identity where
  read _ = pure "content from file"


class Monad m => HasRandom m where
  random :: Random a => (a, a) -> m a

instance HasRandom IO where
  random = randomRIO

instance HasRandom Identity where
  random = pure . fst


quote :: Char -> String -> String
quote c str = [c] <> str <> [c]

quoteAnswer :: Bool -> String -> String
quoteAnswer False answer = quote '"' answer <> " was a horrible answer" 
quoteAnswer True  answer = quote '"' answer <> " was a wonderful answer" 


makeQuestion :: (HasConsole m, HasFile m) => m String
makeQuestion = do
  question <- read "question.txt"
  output question
  answer <- input
  if answer == "42"
    then output "correct!"
    else output "incorrect"
  pure answer

program :: (HasRandom m, HasConsole m, HasFile m) => m ()
program = do
  answer <- makeQuestion
  nice <- random (False, True)
  output $ quoteAnswer nice answer

run :: IO ()
run = program

test :: Identity ()
test = program
