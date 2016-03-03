{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.ByteString (ByteString)
import           Data.ByteString.Char8 (unpack, pack)
import           Data.IORef (IORef, newIORef, modifyIORef, readIORef)
import           Foreign.C.Types (CInt)
import           Scripting.Lua (LuaState)
import qualified Scripting.Lua as Lua
import           System.Environment (getArgs)

-- Callback functions are integers, for reasons I'll explain
-- a bit later.
type Function = Int

-- we're using a fake version of a 'job' here that has a
-- name, a (possibly empty, optional) list of dependencies,
-- a 'return value' that's just a string, and the (fake)
-- 'command' that's run. Crucially, the command can rely on
-- the 'retun value' from previous steps.
data Job = Job
  { jobName :: String
  , jobDeps :: [String]
  , jobRet  :: String
  , jobCmd  :: Either String Int
  } deriving Show

-- The result of running a job names the job it resulted
-- from and gives a value.
data Result = Result
  { resultName :: String
  , resultVal  :: String
  }

-- This is a Very Bad function. In particular, if the
-- dependency graph of jobs is ill-formed, it'll just run
-- forever. Also, it's super inefficient.
runJobs :: LuaState -> [Job] -> [Result] -> IO ()
runJobs l [] _ = putStrLn "done with jobs"
runJobs l (j:js) prev
  -- if all of our dependencies appear in our 'results' list,
  -- then all dependencies have been met.
  | and [ d `elem` map resultName prev | d <- jobDeps j] = do
      -- we print some nice stuff
      putStrLn ("running job `" ++ jobName j ++ "'")
      cmd <- runCmd l (jobCmd j) (map resultVal prev)
      putStrLn ("  command is `" ++ cmd ++ "'")
      putStrLn ("  result is `" ++ jobRet j ++ "'")
      -- and add the 'result' to the list of results
      let res = Result (jobName j) (jobRet j)
      runJobs l js (res:prev)
  -- If our dependencies haven't all been met, then add the
  -- current job to the end of the list and keep trying.
  | otherwise = runJobs l (js ++ [j]) prev

-- A command can either be a raw string, OR a function reference.
-- we call into Lua-land in the latter case.
runCmd :: LuaState -> Either String Function -> [String] -> IO String
runCmd _ (Left str) _ = return str
runCmd l (Right f) xs = runCallback l f xs

-- *** HERE BE DRAGONS ***
-- The Lua API in Haskell is gross. It's a very, very thin wrapper
-- over the C API, and for anything non-trivial---and in particular,
-- anything involving tables---we have to use the raw Lua stack.
-- This code was written hastily, and therefore makes some assumptions
-- about the layout of the stack that might not be true in a larger
-- program. Fairly warned be ye!

-- This is a wrapper over an operation we use several times below:
-- grabbing a key out of a table, where that key has a Haskell
-- equivalent, as expressed by a 'StackValue' instance:
getTableVal :: Lua.StackValue a => LuaState -> ByteString -> IO (Maybe a)
getTableVal l key = do
  -- we assume the table is on the stack at position 1, and we
  -- push the key, which is now at position 2
  Lua.pushstring l key
  -- we fetch the value from the table at position 1; this
  -- replaces the string key with the value
  Lua.gettable l 1
  -- we ATTEMPT to convert it to a corresponding Haskell value.
  -- This might.
  res <- Lua.peek l 2
  -- we then pop down to the original table, and return the result.
  Lua.pop l 1
  return res


-- This is the function we expose in Lua-land. It also needs access
-- to the list of jobs we're building in Haskell.
register :: IORef ([Job]) -> LuaState -> IO CInt
register refs l = do
  -- This is basic error-handling: we make sure that we're
  -- passed a table. Simple enough.
  isTable <- Lua.istable l 1
  if not isTable
    then do
      -- If it's not, we just return nothing. It's fine.
      -- This is how programming goes. It's just life.
      putStrLn "Argument is not a table"
      return 0
    else do
      -- Otherwise, we start fetching the values: this will
      -- fail at runtime if there's no `name', key or if the
      -- `name' value is the wrong type. Ideally, this would
      -- be handled better.
      Just name <- getTableVal l "name"
      -- Same, but for 'ret'
      Just ret  <- getTableVal l "ret"
      -- Our `cmd' can be either a string or a function. Here,
      -- we do something a little gross: we speculatively grab
      -- it as a string, which it might not be
      cmdStr <- getTableVal l "cmd"
      -- If it is, then we use that, but if it's NOT...
      cmd <- case cmdStr of
        Just bs -> return (Left (unpack bs))
        Nothing -> do
          -- We grab a global table called `_callbacks', now. at
          -- position 2 in the stack...
          Lua.getglobal l "_callbacks"
          -- And then pull the "command" value out of the table
          -- again...
          Lua.pushstring l "cmd"
          Lua.gettable l 1
          -- "and then, insert that value into the `_callbacks'
          -- table with a new fresh index. That index is exposed
          -- as an integer...
          func <- Lua.ref l 2
          -- Pop back down to the table...
          Lua.pop l 1
          -- And that integer can now serve as our reference to
          -- the created function!
          return (Right func)
      -- Finally, we grab the list of dependencies
      deps <- getTableVal l "deps"
      -- And if it's not there (i.e. if it was nil) then we
      -- default to using an empty list
      let deps' = case deps of
            Nothing -> []
            Just xs -> xs

      -- Add it to the jobs array and we're good for this one!
      putStrLn ("[registering] job " ++ unpack name)
      let job = Job { jobName = unpack name
                    , jobDeps = map unpack deps'
                    , jobCmd  = cmd
                    , jobRet  = unpack ret
                    }
      modifyIORef refs (job:)
      return 0

-- Okay, so here's the deal: lots of Lua values we can serialize
-- directly back into Haskell. Strings, numbers, booleans, nil...
-- but there's no way to directly serialize a Lua function or
-- closure back into a Haskell. That means we have to keep a
-- reference to it SOMEWHERE in Lua-land, which in this case I
-- do with a global table called `_callbacks'. Every time we
-- ran into a function above, I stuck it in there with a new
-- index.

-- That means our Haskell-land representation of a 'function'
-- is an index into the `_callbacks' table. To call one of
-- those:
runCallback :: LuaState -> Function -> [String] -> IO String
runCallback l func args = do
  -- we first grab the callbacks table, which will now be
  -- at position 1 in the stack...
  Lua.getglobal l "_callbacks"
  -- and then we index into that table to pull out our
  -- function, which is now at position 2.
  Lua.rawgeti l 1 func
  -- Our functions will take as many arguments as they
  -- need, which all get pushed above the function.
  mapM (Lua.pushstring l . pack) args
  -- We invoke the function, which has `len args' number
  -- of arguments and 1 return value (the string). We
  -- SHOULD check the error value here, but, eh. Whatevs.
  _ <- Lua.pcall l (length args) 1 1
  -- We then grab that return value, assuming it'll be
  -- a string. Again. Error-handling is a good thing that
  -- I would do in a real program, but this is kind of a
  -- demo thing.
  Just res <- Lua.peek l (-1)
  -- pop everything back on down...
  Lua.pop l 1
  -- and return the resulting string!
  return (unpack res)

-- Actually setting up Lua to run is easy:
main :: IO ()
main = do
  -- We create a state (e.g. stack, GC state, etc)
  l <- Lua.newstate
  -- And an IORef'ed list for our jobs
  jobs <- newIORef []
  -- We create the `_callbacks' table...
  Lua.newtable l
  Lua.setglobal l "_callbacks"
  -- and register our, uh, `register' function so that
  -- it appears in Lua-land
  Lua.registerrawhsfunction l "register" (register jobs)
  -- Load up a sample source file:
  filename:_ <- getArgs
  Lua.loadfile l filename
  -- Loading a file puts a code representation of it on the
  -- stack, but we still need to call it:
  err <- Lua.pcall l 0 0 0
  -- if there was an error, then we say so and print the
  -- nice string representation of it...
  if err > 0 then do
      rs <- Lua.tostring (l) (-1)
      print rs
    else do
      -- otherwise, grab the jobs list and run them!
      js <- readIORef jobs
      putStrLn ""
      runJobs l js []
  -- You should always leave the campsite exactly as you found
  -- it, and throw away any file descriptors in case bears get
  -- to them.
  Lua.close l
