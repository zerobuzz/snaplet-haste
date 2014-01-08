{-# language OverloadedStrings #-}

-- | NOTE: documentation is obsolete.  this is a clone of the
--   snaplet-haste tool that works for typescript code only, and only
--   for a very special setup.  a more general version for arbitrary
--   to-js setups is somewhere deep down on some todo list.
--
--
--
--   Snaplet that serves javascript files compiled with haste
--   (<https://github.com/valderman/haste-compiler>). This Snaplet is meant to be
--   used for development. You can work on client side Haskell code and
--   immedietely test the code with a simple browser reload. It certainly adds
--   some overhead and is not meant to be used in production websites.
--
-- Usage:
--
-- Put haskell source files in the snaplet path (e.g. @$ROOT\/snaplets\/haste@).
-- For every such haskell file there will be a new javascript file available via
-- http.
--
-- * Other files won't be served through http. The snaplet will 'mzero' on .hs,
--   .hi, .o and all other files.
--
-- * If any haskell file in the snaplet path is newer than the
--   requested javascript file, it will be recompiled.  The haste
--   snaplet does not track haskell import dependencies: recompilation
--   happens whether the js file is requested or not.
--
-- * If hastec exits with an error code this snaplet will serve a special
--   javascript file that contains the error message as a comment and a
--   javascript command that will raise the error message as a javascript
--   exception.
--
-- Please add such a handler to your routes:
--
--   [ ...
--   , ("" , with haste hasteServe)
--   ]

module Snap.Snaplet.CompileJS (
    CompileJS,
    initialize,
    jsServe,
  ) where


import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Class as State
import Data.List
import Data.String.Conversions
import Snap.Core
import Snap.Snaplet
import Snap.Util.FileServe
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Process
import Text.Printf


sourceExtension :: String
sourceExtension = "ts"

compilerPath :: FilePath
compilerPath = "typescript"

compilerArgs :: [String]
compilerArgs = []


whenM :: Monad m => m Bool -> m () -> m ()
whenM condition action = condition >>= (`when` action)

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM condition action = condition >>= (`unless` action)


-- | Internal data type for the compilejs snaplet.
data CompileJS = CompileJS FilePath [String]

-- | Initializes the compilejs snaplet. Use it with e.g. 'nestSnaplet'.
initialize :: SnapletInit app CompileJS
initialize = makeSnaplet "typescript" description Nothing $ do
    return $ CompileJS compilerPath compilerArgs
  where
    description = "handler for delivering javascript files compiled with typescript"

jsServe :: Handler app CompileJS ()
jsServe = do
    jsPath <- cs <$> rqPathInfo <$> getRequest
    case splitExtension jsPath of
        (basename, ".js") -> deliverJS basename
        _ -> mzero

deliverJS :: FilePath -> Handler app CompileJS ()
deliverJS basename = do
    snapletDir <- getSnapletFilePath
    let ts = snapletDir </> basename <.> "ts"
        js = snapletDir </> basename <.> "js"

    liftIO $ doesFileExist ts             `unlessM`  mzero
    liftIO (isSourceNewer js snapletDir)  `whenM`    compile basename
    serveFile (basename <.> "js")


-- | If a given js file exists, return if any source file in the given
-- directory (file ending in 'sourceExtension') is newer than that
-- file.  If js file or directory do not exist, crash.
isSourceNewer :: FilePath -> FilePath -> IO Bool
isSourceNewer jsFile dir = do
    liftIO (doesFileExist jsFile)
        `unlessM` error ("isSourceNewer: could not find " ++ show jsFile)
    liftIO (doesDirectoryExist jsFile)
        `unlessM` error ("isSourceNewer: could not find " ++ show dir)

    sourceFiles <- collectAllSourceFiles dir
    sourceTimeStamps <- mapM getModificationTime sourceFiles
    jsTimeStamp <- getModificationTime jsFile
    return (jsTimeStamp > maximum sourceTimeStamps)
  where
    collectAllSourceFiles :: FilePath -> IO [FilePath]
    collectAllSourceFiles dir = do
        paths <- fmap (dir </>) <$>
            filter (not . (`elem` [".", ".."])) <$>
            getDirectoryContents dir
        (files, dirs) <- partitionM doesFileExist paths
        let sourceFiles = filter (\ f -> ("." <> takeExtension f) == sourceExtension) files
        subSourceFiles <- concat <$> mapM collectAllSourceFiles dirs
        return (sourceFiles ++ subSourceFiles)

    partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
    partitionM pred (a : r) = do
        is <- pred a
        (yes, no) <- partitionM pred r
        return $ if is then (a : yes, no) else (yes, a : no)
    partitionM pred [] = return ([], [])

-- | Recompiles the file and serves it in case of success.
compile :: FilePath -> Handler app CompileJS ()
compile name = do
    CompileJS compilerPath compilerArgs <- State.get
    dir <- getSnapletFilePath
    (exitCode, message) <- liftIO $ do
        (_stdin, Just stdoutH, Just stderrH, processHandle) <-
            createProcess (proc compilerPath $ compilerArgs ++ [name <.> sourceExtension]){
                cwd = Just dir,
                std_out = CreatePipe,
                std_err = CreatePipe
              }
        exitCode <- waitForProcess processHandle
        stdout <- hGetContents stdoutH
        stderr <- hGetContents stderrH
        return (exitCode, "\n*** error compiling to javascript!\nstdout:\n" ++ stdout ++ "\nstderr:\n" ++ stderr)
    case exitCode of
        ExitSuccess -> return ()
        ExitFailure _ -> do
            modifyResponse (setResponseCode 500)
            writeBS $ cs (printf ("/*\n\n%s\n\n*/\n\nthrow %s;") message (show message) :: String)
            getResponse >>= finishWith
