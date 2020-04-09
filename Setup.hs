module Main
    ( main
    )
where

import Data.Maybe
import Distribution.Simple
import Distribution.Simple.Utils
import Distribution.Types.LocalBuildInfo
import Distribution.Verbosity
import System.FilePath

main :: IO ()
main = defaultMainWithHooks simpleUserHooks { postBuild = postBuildHook }

postBuildHook :: a -> b -> c -> LocalBuildInfo -> IO ()
postBuildHook _ _ _ info = do
    let packageDir = takeDirectory $ fromJust $ pkgDescrFile info
    let jsexe = buildDir info </> "abacus" </> "abacus.jsexe"
    copyDirectoryRecursive verbose
        (packageDir </> "node_modules" </> "katex" </> "dist")
        (jsexe </> "katex")
