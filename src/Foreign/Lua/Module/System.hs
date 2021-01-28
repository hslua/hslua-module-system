{-|
Module      : Foreign.Lua.Module.System
Copyright   : © 2019-2020 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert+hslua@zeitkraut.de>
Stability   : alpha
Portability : Requires GHC 8 or later.

Provide a Lua module containing a selection of @'System'@ functions.
-}
module Foreign.Lua.Module.System (

  -- * Module
    pushModule
  , preloadModule

  -- * Fields
  , arch
  , compiler_name
  , compiler_version
  , os

  -- * Functions
  , env
  , getwd
  , getenv
  , ls
  , mkdir
  , rmdir
  , setenv
  , setwd
  , tmpdirname
  , with_env
  , with_tmpdir
  , with_wd
  )
where

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Control.Monad.Catch (bracket)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Version (versionBranch)
import Foreign.Lua (Lua, NumResults (..), Peekable, Pushable, StackIndex)
import Foreign.Lua.Call hiding (render)
import Foreign.Lua.Module hiding (preloadModule, pushModule, render)
import Foreign.Lua.Peek
  (Peeker, peekIntegral, peekList, peekString, peekText, toPeeker)
import Foreign.Lua.Push (Pusher, pushBool, pushIntegral, pushText)
import Foreign.Lua.Module.SystemUtils

import qualified Data.Map as Map
import qualified Foreign.Lua as Lua
import qualified System.Directory as Directory
import qualified System.Environment as Env
import qualified System.Info as Info
import qualified System.IO.Temp as Temp

--
-- Module
--

-- | Textual description of the "doclayout" module.
description :: Text
description = "Plain-text document layouting."

-- | Self-documenting module.
documentedModule :: Module
documentedModule = Module
  { moduleName = "system"
  , moduleFields = fields
  , moduleDescription = description
  , moduleFunctions = functions
  }

-- | Pushes the @system@ module to the Lua stack.
pushModule :: Lua NumResults
pushModule = 1 <$ pushModule' documentedModule

pushModule' :: Module -> Lua ()
pushModule' mdl = do
  Module.pushModule mdl
  forM_ (moduleFields mdl) $ \field -> do
    pushText (fieldName field)
    fieldPushValue field
    Lua.rawset (Lua.nth 3)

-- | Add the @system@ module under the given name to the table
-- of preloaded packages.
preloadModule :: String -> Lua ()
preloadModule name = Module.preloadModule $
  documentedModule { moduleName = T.pack name }

--
-- Fields
--

-- | Exposed fields.
fields :: [Field]
fields =
  [ arch
  , compiler_name
  , compiler_version
  , os
  ]

--
-- Fields
--

-- | Module field containing the machine architecture on which the
-- program is running. Wraps @'Info.arch'@
arch :: Field
arch = Field
  { fieldName = "arch"
  , fieldDescription = "The machine architecture on which the program "
                       <> "is running."
  , fieldPushValue = pushString Info.arch
  }

-- | Module field containing the Haskell implementation with which the
-- host program was compiled. Wraps @'Info.compilerName'@.
compiler_name :: Field
compiler_name = Field
  { fieldName = "compiler_name"
  , fieldDescription = "The Haskell implementation with which the host "
                       <> "program was compiled."
  , fieldPushValue = pushString Info.compilerName
  }

-- | Module field containing the version of `compiler_name` with which
-- the host program was compiled.
compiler_version :: Field
compiler_version = Field
  { fieldName = "compiler_version"
  , fieldDescription = "The Haskell implementation with which the host "
                       <> "program was compiled."
  , fieldPushValue = pushList pushIntegral Info.compilerVersion
  }

-- | Field containing the operating system on which the program is
-- running.
os :: Field
os = Field
  { fieldName = "os"
  , fieldDescription = "The operating system on which the program is "
                       <> "running."
  , fieldPushValue = pushString Info.os
  }


--
-- Functions
--

-- | Retrieve the entire environment
env :: Lua NumResults
env = HaskellFunction
  { callFunction = do
      kvs <- ioToLua Env.getEnvironment
      let addValue (k, v) = Lua.push k *> Lua.push v *> Lua.rawset (-3)
      Lua.newtable
      mapM_ addValue kvs
      return (NumResults 1)
  , functionDoc = Just $ FunctionDoc
    { functionDescription = ""
    , parameterDocs = []
    , functionResultDocs = []
    }
  }

-- | Return the current working directory as an absolute path.
getwd :: Lua FilePath
getwd = ioToLua Directory.getCurrentDirectory

-- | Returns the value of an environment variable
getenv :: String -> Lua (Optional String)
getenv name = ioToLua (Optional <$> Env.lookupEnv name)

-- | List the contents of a directory.
ls :: Optional FilePath -> Lua [FilePath]
ls fp = do
  let fp' = fromMaybe "." (fromOptional fp)
  ioToLua (Directory.listDirectory fp')

-- | Create a new directory which is initially empty, or as near to
-- empty as the operating system allows.
--
-- If the optional second parameter is `false`, then create the new
-- directory only if it doesn't exist yet. If the parameter is `true`,
-- then parent directories are created as necessary.
mkdir :: FilePath -> Bool -> Lua ()
mkdir fp createParent =
  if createParent
  then ioToLua (Directory.createDirectoryIfMissing True fp)
  else ioToLua (Directory.createDirectory fp)

-- | Remove an existing directory.
rmdir :: FilePath -> Bool -> Lua ()
rmdir fp recursive =
  if recursive
  then ioToLua (Directory.removeDirectoryRecursive fp)
  else ioToLua (Directory.removeDirectory fp)

-- | Set the specified environment variable to a new value.
setenv :: String -> String -> Lua ()
setenv name value = ioToLua (Env.setEnv name value)

-- | Change current working directory.
setwd :: FilePath -> Lua ()
setwd fp = ioToLua $ Directory.setCurrentDirectory fp

-- | Get the current directory for temporary files.
tmpdirname :: Lua FilePath
tmpdirname = ioToLua Directory.getTemporaryDirectory

-- | Run an action in a different directory, then restore the old
-- working directory.
with_wd :: FilePath -> Callback -> Lua NumResults
with_wd fp callback =
  bracket (Lua.liftIO Directory.getCurrentDirectory)
          (Lua.liftIO . Directory.setCurrentDirectory)
          $ \_ -> do
              Lua.liftIO (Directory.setCurrentDirectory fp)
              callback `invokeWithFilePath` fp


-- | Run an action, then restore the old environment variable values.
with_env :: Map.Map String String -> Callback -> Lua NumResults
with_env environment callback =
  bracket (Lua.liftIO Env.getEnvironment)
          setEnvironment
          (\_ -> setEnvironment (Map.toList environment) >> invoke callback)
 where
  setEnvironment newEnv = Lua.liftIO $ do
    -- Crude, but fast enough: delete all entries in new environment,
    -- then restore old environment one-by-one.
    curEnv <- Env.getEnvironment
    forM_ curEnv (Env.unsetEnv . fst)
    forM_ newEnv (uncurry Env.setEnv)

with_tmpdir :: String            -- ^ parent dir or template
            -> AnyValue          -- ^ template or callback
            -> Optional Callback -- ^ callback or nil
            -> Lua NumResults
with_tmpdir parentDir tmpl callback =
  case fromOptional callback of
    Nothing -> do
      -- At most two args. The first arg (parent dir) has probably been
      -- omitted, so we shift arguments and use the system's canonical
      -- temporary directory.
      let tmpl' = parentDir
      callback' <- Lua.peek (fromAnyValue tmpl)
      Temp.withSystemTempDirectory tmpl' (invokeWithFilePath callback')
    Just callback' -> do
      -- all args given. Second value must be converted to a string.
      tmpl' <- Lua.peek (fromAnyValue tmpl)
      Temp.withTempDirectory parentDir tmpl' (invokeWithFilePath callback')
