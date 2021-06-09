{-#
LANGUAGE TemplateHaskell, GADTs, FlexibleContexts, DataKinds, RankNTypes
#-}

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

pureConsole :: Console a -> a
pureConsole Input = "user input"
pureConsole (Output str) = ()


data FileSystem a where
  Read  :: FilePath -> FileSystem String
  Write :: FilePath -> String -> FileSystem ()

makeEffect ''FileSystem

filesystem :: FileSystem a -> IO a
filesystem (Read path) = readFile path
filesystem (Write path content) = writeFile path content

pureFilesystem :: FileSystem a -> a
pureFilesystem (Read _)    = "mocked file"
pureFilesystem (Write _ _) = ()


data Randomness a where
  Random :: R.Random r => (r, r) -> Randomness r

makeEffect ''Randomness

randomness :: Randomness a -> IO a
randomness (Random range) = randomRIO range

pureRandomness :: Randomness a -> a
pureRandomness (Random (min, _)) = min


type Effects =
  '[ Randomness
   , Console
   , FileSystem
   ]

type App a = forall effs. Members Effects effs => Eff effs a


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

program :: App ()
program = do
  answer <- makeQuestion
  nice <- random (False, True)
  output $ quoteAnswer nice answer


runProgram :: IO ()
runProgram = program
  & interpretM randomness
  & interpretM filesystem
  & interpretM console
  & runM

pureProgram :: ()
pureProgram = program
  & interpret (pure . pureRandomness)
  & interpret (pure . pureFilesystem)
  & interpret (pure . pureConsole)
  & run
