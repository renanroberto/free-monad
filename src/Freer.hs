{-# LANGUAGE GADTs, FlexibleContexts, DataKinds, TemplateHaskell #-}

module Freer where

import Prelude hiding (read)
import Data.Function ((&))
import Data.Functor.Identity
import Data.IORef
import Control.Monad.Freer
import Control.Monad.Freer.TH (makeEffect)
import System.Random (randomRIO)
import qualified System.Random as R (Random)


data Console a where
  Input  :: Console String
  Output :: String -> Console ()

makeEffect ''Console

console :: Console a -> IO a
console Input = getLine
console (Output str) = putStrLn str

pureConsole :: Console a -> Identity a
pureConsole Input = pure "user input"
pureConsole (Output str) = pure ()


data FileSystem a where
  Read  :: FilePath -> FileSystem String
  Write :: FilePath -> String -> FileSystem ()

makeEffect ''FileSystem

fileSystem :: FileSystem a -> IO a
fileSystem (Read path) = readFile path
fileSystem (Write path content) = writeFile path content

pureFileSystem :: FileSystem a -> Identity a
pureFileSystem (Read path) = pure path
pureFileSystem (Write path content) = pure ()


initialState :: IO (IORef String)
initialState = newIORef "random question?"

memoryRef :: IORef String -> FileSystem a -> IO a
memoryRef ref (Read _)  = readIORef ref
memoryRef ref (Write _ content) = writeIORef ref content

memory :: FileSystem a -> IO a
memory action = do
  ref <- initialState
  memoryRef ref action


data Randomness a where
  Random :: R.Random r => (r, r) -> Randomness r

makeEffect ''Randomness

randomness :: Randomness a -> IO a
randomness (Random range) = randomRIO range

pureRandomness :: Randomness a -> Identity a
pureRandomness (Random (min, _)) = pure min


quote :: Char -> String -> String
quote c str = [c] <> str <> [c]

quoteAnswer :: Bool -> String -> String
quoteAnswer False answer = quote '"' answer <> " was a horrible answer" 
quoteAnswer True  answer = quote '"' answer <> " was a wonderful answer" 


makeQuestion :: Members '[Console, FileSystem] effs => Eff effs String
makeQuestion = do
  question <- read "question.txt"
  output question
  answer <- input
  if answer == "42"
    then output "Correct!"
    else output "Incorrect"
  pure answer

program :: Members '[Randomness, Console, FileSystem] effs => Eff effs ()
program = do
  answer <- makeQuestion
  nice <- random (False, True)
  output $ quoteAnswer nice answer


runProgram :: IO ()
runProgram = program
  & interpretM randomness
  & interpretM memory
  & interpretM console
  & runM

pureProgram :: ()
pureProgram = program
  & interpretM pureRandomness
  & interpretM pureFileSystem
  & interpretM pureConsole
  & runM
  & runIdentity
