{-# LANGUAGE OverloadedStrings #-}

import Data.Yaml (decode)
import Data.Map (Map, toList)

import qualified Data.ByteString.Char8 as BS
import System.FSNotify
import System.Process
import System.Directory
import Data.String.Utils (replace)
import Data.List
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import System.Exit (ExitCode)
import Text.Regex.Posix
import System.Path.WildMatch as WM
import Debug.Trace (putTraceMsg)

-- pair with glob string and list of command strings
type DispatchDef = (String,[String])

objectToADList Nothing = []
objectToADList (Just x) = toList x

-- Given the path to an Hctawfile (which is in yaml format)
-- Returns [(globString, [commandString])]
getADList :: String -> IO [DispatchDef]
getADList fsPath = do
    -- Todo: add exception with nice message if the file isn't present https://www.schoolofhaskell.com/user/snoyberg/general-haskell/exceptions/catching-all-exceptions
    ymlData <- BS.readFile fsPath
    -- Parse the file, it's expected to be in a particular format
    let adl = decode ymlData :: Maybe (Map String [String]) 
    case adl of
        -- If parse is successful, it will return a map, convert it to an associate list
        Just _  -> return $ objectToADList adl
        Nothing -> error "Could not parse YAML file."

main :: IO ()
main = do
  -- Perform an IO action with an fsnotify WatchManager in place.
  -- Tear down the WatchManager after the action is complete.
  -- https://hackage.haskell.org/package/fsnotify-0.2.1.1/docs/System-FSNotify.html
  withManagerConf defaultConfig { confDebounce = Debounce (1/2)} $ \mgr -> do
    putStrLn "Manager started."

    cwd <- getCurrentDirectory
    putStrLn cwd
    
    actionList <- getADList "Hctawfile"
    putStrLn $ show actionList

    -- Convert the config into a big function that acts on an event.
    let pathSpecificAction = genericAction actionList (cwd ++ "/")

    -- Watch for change (this blocks)
    watchTree
      mgr          -- manager
      "."          -- directory to watch
      (const True) -- predicate, const x is a unary function which evaluates to x for all inputs.
      pathSpecificAction    -- big function that matches event's path to glob and runs actions

    forever $ threadDelay 100000 -- wait 1/10 second and rerun


-- Equivalent to bifunctor first
modifyFst f (x, y) = ((f x), y)


-- Takes: [(glob string,[command string])], (the path in which hctaw is runing), a file system event
-- Runs all applicable (as determined by globs) commands, ignores results.
genericAction :: [DispatchDef] -> String -> Event -> IO ()
genericAction actionDispatch basePath e = do
    -- convert first in pairs, glob strings, to matcher functions
    let actionDispatch' = map (modifyFst WM.wildCheckCase) actionDispatch

    -- apply eventPath to event, giving us a file path string then strip; then absolute path.
    let relativeFilePath = replace basePath "" $ eventPath e

    -- filter the list of pairs (glob, [commands]), isolate the lists of commands;
    -- then concat them into a big list of commands (who's glob matches the event's path)
    let commandStrings = concat $ map snd $ filter (\ t -> (fst t) relativeFilePath ) actionDispatch'
    -- Replace occurences of the token string "${FILE}" to the file path.
    let commandStrings' = map (replace "${FILE}" relativeFilePath) commandStrings

    -- Convert the command strings to IO actions
    let commands = makeShellCommands commandStrings'

    -- Run the IO actions and ignore their result success/failure
    sequence_ commands
    return ()

-- terminateProcess
-- spawnCommand String -> IO ProcessHandle
-- Takes a string and outputs an action that runs the string as a shell command
makeShellCommands [] = map system [":"] -- system :: String -> IO ExitCode
makeShellCommands x = 
    map system x

