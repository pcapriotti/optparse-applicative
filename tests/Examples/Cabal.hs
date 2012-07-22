{-# LANGUAGE Arrows #-}
module Examples.Cabal where

import Options.Applicative
import Options.Applicative.Arrows

data Args = Args CommonOpts Command
  deriving Show

data CommonOpts = CommonOpts
  { optVerbosity :: Int }
  deriving Show

data Command
  = Install ConfigureOpts InstallOpts
  | Update
  | Configure ConfigureOpts
  | Build BuildOpts
  deriving Show

data InstallOpts = InstallOpts
  { instReinstall :: Bool
  , instForce :: Bool }
  deriving Show

data ConfigureOpts = ConfigureOpts
  { configTests :: Bool
  , configFlags :: [String] }
  deriving Show

data BuildOpts = BuildOpts
  { buildDir :: FilePath }
  deriving Show

parser :: Parser Args
parser = runA $ proc () -> do
  opts <- asA commonOpts -< ()
  cmds <- (asA . subparser)
            ( command "install"
              (info installParser
                    (progDesc "Installs a list of packages"))
            & command "update"
              (info updateParser
                    (progDesc "Updates list of known packages"))
            & command "configure"
              (info configureParser
                    (progDesc "Prepare to build the package"))
            & command "build"
              (info buildParser
                    (progDesc "Make this package ready for installation")) ) -< ()
  A helper -< Args opts cmds

commonOpts :: Parser CommonOpts
commonOpts = CommonOpts
  <$> option
      ( short 'v'
      & long "verbose"
      & metavar "LEVEL"
      & help "Set verbosity to LEVEL"
      & value 0 )

installParser :: Parser Command
installParser = runA $ proc () -> do
  config <- asA configureOpts -< ()
  inst <- asA installOpts -< ()
  A helper -< Install config inst

installOpts :: Parser InstallOpts
installOpts = runA $ proc () -> do
  reinst <- asA (switch (long "reinstall")) -< ()
  force <- asA (switch (long "force-reinstall")) -< ()
  returnA -< InstallOpts
             { instReinstall = reinst
             , instForce = force }

updateParser :: Parser Command
updateParser = runA $ proc () ->
  A helper -< Update

configureParser :: Parser Command
configureParser = runA $ proc () -> do
  config <- asA configureOpts -< ()
  A helper -< Configure config

configureOpts :: Parser ConfigureOpts
configureOpts = runA $ proc () -> do
  tests <- (asA . switch)
             ( long "enable-tests"
             & help "Enable compilation of test suites" ) -< ()
  flags <- (asA . many . strOption)
             ( short 'f'
             & long "flags"
             & metavar "FLAGS"
             & help "Enable the given flag" ) -< ()
  returnA -< ConfigureOpts tests flags

buildParser :: Parser Command
buildParser = runA $ proc () -> do
  opts <- asA buildOpts -< ()
  A helper -< Build opts

buildOpts :: Parser BuildOpts
buildOpts = runA $ proc () -> do
  bdir <- (asA . strOption)
            ( long "builddir"
            & metavar "DIR"
            & value "dist" ) -< ()
  returnA -< BuildOpts bdir

pinfo :: ParserInfo Args
pinfo = info parser idm

main :: IO ()
main = do
  r <- execParser pinfo
  print r
