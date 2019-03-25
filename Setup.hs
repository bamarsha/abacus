import Distribution.Simple (Args, defaultMainWithHooks, postBuild,
                            simpleUserHooks)
import Distribution.Simple.Setup (BuildFlags, buildProgramPaths)
import Distribution.Types.Executable (exeName)
import Distribution.Types.LocalBuildInfo (LocalBuildInfo, buildDir)
import Distribution.Types.PackageDescription (PackageDescription, executables)
import Distribution.Types.UnqualComponentName (unUnqualComponentName)

import System.Directory (copyFile, exeExtension)
import System.FilePath ((</>), (<.>))

-- Runs the custom build with hooks.
main :: IO ()
main = defaultMainWithHooks simpleUserHooks { postBuild = postBuildHook }

-- Copies executables to the current directory after they're built.
postBuildHook :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo
              -> IO ()
postBuildHook _ _ desc info = do
  let dir = buildDir info
  let exePaths = map (\exe -> let name = unUnqualComponentName (exeName exe)
                              in (dir </> name </> name <.> exeExtension,
                                  name <.> exeExtension))
                     (executables desc)
  mapM_ (uncurry copyFile) exePaths
