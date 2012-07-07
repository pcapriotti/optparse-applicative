module Examples.Cabal where

import Options.Applicative

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

withHelp :: Parser a -> Parser a
withHelp = (helper <*>)

parser :: Parser Args
parser = withHelp $ Args <$> commonOpts <*> subparser
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
          (progDesc "Make this package ready for installation")) )

commonOpts :: Parser CommonOpts
commonOpts = CommonOpts
  <$> option
      ( short 'v'
      & long "verbose"
      & metavar "LEVEL"
      & help "Set verbosity to LEVEL"
      & value 0 )

installParser :: Parser Command
installParser = withHelp
  $ Install <$> configureOpts <*> installOpts

installOpts :: Parser InstallOpts
installOpts = InstallOpts
  <$> switch ( long "reinstall" )
  <*> switch ( long "force-reinstall" )

updateParser :: Parser Command
updateParser = withHelp
  $ pure Update

configureParser :: Parser Command
configureParser = withHelp
  $ Configure <$> configureOpts

configureOpts :: Parser ConfigureOpts
configureOpts = ConfigureOpts
  <$> switch ( long "enable-tests"
             & help "Enable compilation of test suites" )
  <*> strOption ( short 'f'
                & long "flags"
                & metavar "FLAGS"
                & help "Enable the given flag"
                & multi )

buildParser :: Parser Command
buildParser = helper <*> (Build <$> buildOpts)

buildOpts :: Parser BuildOpts
buildOpts = BuildOpts
  <$> strOption
      ( long "builddir"
      & metavar "DIR"
      & value "dist" )

pinfo :: ParserInfo Args
pinfo = info parser idm

main :: IO ()
main = do
  r <- execParser pinfo
  print r
