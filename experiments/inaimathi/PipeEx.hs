module Main where

import Control.Monad (unless, forever)
import Pipes
-- import Pipes.Concurrent
import System.IO (isEOF)

import Control.Exception (try, throwIO)
import qualified GHC.IO.Exception as G

stdinLn :: Producer String IO ()
stdinLn = do
  eof <- lift isEOF
  unless eof $ do
            str <- lift getLine
            yield str
            stdinLn

stdoutLn :: Show a => Consumer a IO ()
stdoutLn = do
  msg <- await  -- 'await' an a
  x   <- lift $ try $ putStrLn $ show msg
  case x of
    -- Gracefully terminate if we got a broken pipe error
    Left e@(G.IOError { G.ioe_type = t}) ->
           lift $ unless (t == G.ResourceVanished) $ throwIO e
         -- Otherwise loop
    Right () -> stdoutLn

pairer :: (Monad m) => Pipe a (a, a) m ()
pairer = forever $ do
           a <- await
           b <- await
           yield (a, b)

splitter :: (Monad m) => Pipe [Char] Char m ()
splitter = forever $ do
             str <- await
             mapM_ yield str

tap :: Show a => String -> Pipe a a IO ()
tap label = forever $ do
              a <- await
              lift $ putStrLn $ concat [label, "::> ", show a]
              yield a
              

main :: IO ()
main = runEffect $ stdinLn >-> tap "One" 
       >-> splitter >-> tap "Two" 
       >-> pairer >-> tap "Three" 
       >-> stdoutLn
