module MonadTransformer where


data Identity a =
  Identity { runIdentity :: a }

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure x = Identity x
  mf <*> mx =
    let
      x = runIdentity mx
      f = runIdentity mf
    in Identity (f x)

instance Monad Identity where
  mx >>= f =
    let x = runIdentity mx
    in f x


data StateT s m a =
  StateT { runStateT :: s -> m (a, s) }

instance Monad m => Functor (StateT s m) where
  fmap f st = StateT $ \s -> do
    (x, s') <- runStateT st s
    pure (f x, s')

instance Monad m => Applicative (StateT s m) where
  pure x = StateT $ \s -> pure (x, s)
  sf <*> sx = StateT $ \s -> do
    (f, s')  <- runStateT sf s
    (x, s'') <- runStateT sx s'
    pure (f x, s'')

instance Monad m => Monad (StateT s m) where
  sx >>= f = StateT $ \s -> do
    (x, s') <- runStateT sx s
    (y, s'') <- runStateT (f x) s'
    pure (y, s'')

get :: Monad m => StateT s m s
get = StateT $ \s -> return (s, s)

put :: Monad m => s -> StateT s m ()
put s = StateT $ \_ -> return ((), s)

execStateT :: Monad m => StateT s m a -> s -> m s
execStateT st s = snd <$> runStateT st s

liftState :: Monad m => m a -> StateT s m a
liftState mx = StateT $ \s -> fmap (\x -> (x, s)) mx

type State s a = StateT s Identity a

runState :: State s a -> s -> (a, s)
runState st s = runIdentity (runStateT st s)

execState :: State s a -> s -> s
execState st s = snd $ runState st s


data Writer w s =
  Writer { runWriter :: (s, w) }

instance Functor (Writer w) where
  fmap f ws =
    let (s, w) = runWriter ws
    in Writer (f s, w)

instance Monoid w => Applicative (Writer w) where
  pure s = Writer (s, mempty)
  wf <*> ws =
    let
      (f, w)  = runWriter wf
      (s, w') = runWriter ws
    in Writer (f s, w <> w')

instance Monoid w => Monad (Writer w) where
  ws >>= f =
    let
      (s, w) = runWriter ws
      (s', w') = runWriter (f s)
    in
      Writer (s', w <> w')

tell :: w -> Writer w ()
tell w = Writer ((), w)


data Bank = Bank Int

type Log = (Int, Bank, String)
type Logs = [Log]
type Logger = Writer Logs

readLog :: Log -> String
readLog (amount, Bank balance, msg) =
  "valor: " <> readMoney amount <> "\t"
  <> "saldo: " <> readMoney balance <> "\t"
  <> "mensagem: " <> msg

readMoney :: Int -> String
readMoney = show . (/ 100) . fromIntegral


initial :: Bank
initial = Bank 10000

withdraw :: Int -> StateT Bank Logger (Maybe Int)
withdraw amount = do
  Bank balance <- get
  if amount < balance
    then do
      put $ Bank (balance - amount)
      liftState $ tell [(amount, Bank balance, "saque realizado")]
      pure (Just amount)
    else do
      liftState $ tell [(amount, Bank balance, "saldo insuficiente")]
      pure Nothing

deposit :: Int -> StateT Bank Logger ()
deposit amount = do
  Bank balance <- get
  put $ Bank (balance + amount)
  liftState $ tell [(amount, Bank balance, "deposito realizado")]

operations :: StateT Bank Logger ()
operations =
  withdraw 2500
  >> withdraw 7600
  >> withdraw 2500
  >> withdraw 6000
  >> deposit 2000


runBank :: (Bank, Logs)
runBank = runWriter $ execStateT operations initial


run :: IO ()
run = do
  let (Bank balance, logs) = runBank
  putStrLn $ "Voce tem R$" <> readMoney balance <> " em conta"
  putStrLn "--- Historico ---"
  putStrLn . unlines . fmap readLog $ logs
