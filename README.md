# Applicative option parser

This package contains utilities and combinators to define command line option
parsers.

[![Continuous Integration status][status-png]][status]
[![Hackage page (downloads and API reference)][hackage-png]][hackage]

**Table of Contents**

- [Getting started](#getting-started)
- [Supported options](#supported-options)
    - [Regular options](#regular-options)
    - [Flags](#flags)
    - [Arguments](#arguments)
    - [Commands](#commands)
- [Option builders](#option-builders)
- [Advanced features](#advanced-features)
- [How it works](#how-it-works)

## Getting started

Install with

```sh
cabal install optparse-applicative
```

Here is a simple example of an applicative option parser:

```haskell
import Options.Applicative

data Sample = Sample
  { hello :: String
  , quiet :: Bool }

sample :: Parser Sample
sample = Sample
     <$> strOption
         ( long "hello"
        <> metavar "TARGET"
        <> help "Target for the greeting" )
     <*> switch
         ( long "quiet"
        <> help "Whether to be quiet" )
```

The parser is built using [applicative style][applicative] starting from a set
of basic combinators. In this example, `hello` is defined as an option with a
`String` argument, while `quiet` is a boolean flag (called `switch`).

A parser can be used like this:

```haskell
greet :: Sample -> IO ()
greet (Sample h False) = putStrLn $ "Hello, " ++ h
greet _ = return ()

main :: IO ()
main = execParser opts >>= greet
  where
    opts = info (helper <*> sample)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative" )
```

The `greet` function is the entry point of the program, while `opts` is a
complete description of the program, used when generating a help text. The
`helper` combinator takes any parser, and adds a `help` option to it.

The `hello` option in this example is mandatory (since it doesn't have a
default value), so running the program without any argument will display a
short option summary:

    Usage: hello --hello TARGET [--quiet]

Running the program with the `--help` option will display the full help text:

    hello - a test for optparse-applicative

    Usage: hello --hello TARGET [--quiet]
      Print a greeting for TARGET

    Available options:
      -h,--help                Show this help text
      --hello TARGET           Target for the greeting
      --quiet                  Whether to be quiet

containing a detailed list of options with descriptions.

The specified metavars are used as placeholders for the option arguments, and
can be referred to in the program description.  This makes it possible to
explicitly describe the connection between the options and the behaviour of the
program.

Parsers are instances of both `Applicative` and `Alternative`, and work with
any generic combinator, like `many` and `some`. For example, to make a option
return `Nothing` instead of failing when it's not supplied, you can use the
`optional` combinator in `Control.Applicative`:

```haskell
optional $ strOption
   ( long "output"
  <> metavar "DIRECTORY" )
```

 [applicative]: http://www.soi.city.ac.uk/~ross/papers/Applicative.html

## Supported options

`optparse-applicative` supports four kinds of options: regular options, flags,
arguments and commands.

### Regular options

A **regular option** is an option which takes a single argument, parses it, and
returns a value.

A regular option can have a default value, which is used as the result if the
option is not found in the command line. An option without a default value is
considered mandatory, and produces an error when not found.

Regular options can have **long** names, or **short** (one-character) names,
which determine when the option matches and how the argument is extracted.

An option with a long name (say "output") is specified on the command line as

    --output filename.txt

or

    --output=filename.txt

while a short name option (say "o") can be specified with

    -o filename.txt

or

    -ofilename.txt

Options can have more than one name, usually one long and one short, although
you are free to create options with an arbitrary combination of long and short
names.

Regular options returning strings are the most common, and they can be created
using the `strOption` builder. For example,

```haskell
strOption
   ( long "output"
  <> short 'o'
  <> metavar "FILE"
  <> help "Write output to FILE" )
```

creates a regular option with a string argument (which can be referred to as
`FILE` in the help text and documentation), a long name "output" and a short
name "o". See below for more information on the builder syntax and modifiers.

A regular option can return an object of any type, and takes a *reader*
parameter which specifies how the argument should be parsed.  A common reader is
`auto`, which assumes a `Read` instance for the return type and uses it to parse
its argument. For example:

```haskell
lineCount :: Parser Int
lineCount = option auto
            ( long "lines"
           <> short 'n'
           <> metavar "K"
           <> help "Output the last K lines" )
```

specifies a regular option with an `Int` argument. We added an explicit type
annotation here, since without it the parser would have been polymorphic in the
output type. There's usually no need to add type annotations, however, because
the type will be normally inferred from the context in which the parser is
used.

One can also create a custom reader that doesn't use the `Read` typeclass, and
use it to parse option arguments. A custom reader is a value in the `ReadM`
monad. We provide `eitherReader :: (String -> Either String a) -> ReadM a`
to help create these values, where a `Left` will hold the error message
for a failure.

```haskell
data FluxCapacitor = ...

parseFluxCapacitor :: ReadM FluxCapacitor
parseFluxCapacitor = eitherReader $ \s -> ...

option parseFluxCapacitor ( long "flux-capacitor" )
```

One can also use `ReadM` directly, using `str` to obtain the command line string,
and `readerAbort` or `readerError` within the `ReadM` monad to exit with an
error message.

### Flags

A **flag** is just like a regular option, but it doesn't take any arguments: it is
either present in the command line or not.

A flag has a default value and an **active value**. If the flag is found on the
command line, the active value is returned, otherwise the default value is
used. For example:

```haskell
data Verbosity = Normal | Verbose

flag Normal Verbose
  ( long "verbose"
 <> short 'v'
 <> help "Enable verbose mode" )
```

is a flag parser returning a `Verbosity` value.

Simple boolean flags can be specified using the `switch` builder, like so:

```haskell
switch
  ( long "keep-tmp-files"
 <> help "Retain all intermediate temporary files" )
```

There is also a `flag'` builder, which has no default value. For example, to
add a `--version` switch to a program, you could write:

```haskell
flag' Nothing (long "version" <> hidden) <|> (Just <$> normal_options)
```

### Arguments

An **argument** parser specifies a positional command line argument.

The `argument` builder takes a reader parameter, and creates a parser which
will return the parsed value every time it is passed a command line argument
for which the reader succeeds. For example

```haskell
argument str (metavar "FILE")
```

creates an argument accepting any string.  To accept an arbitrary number of
arguments, combine the `argument` builder with either the `many` or `some`
combinator:

```haskell
some (argument str (metavar "FILES..."))
```

Arguments are only displayed in the brief help text, so there's no need to
attach a description to them. They should be manually documented in the program
description.

Note that arguments starting with `-` are considered options by default, and
will not be considered by an `argument` parser.

However, parsers always accept a special argument: `--`. When a `--` is found on
the command line, all the following words are considered by `argument` parsers,
regardless of whether they start with `-` or not.

### Commands

A **command** can be used to specify a sub-parser to be used when a certain
string is encountered in the command line.

Commands are useful to implement command line programs with multiple functions,
each with its own set of options, and possibly some global options that apply
to all of them. Typical examples are version control systems like `git`, or
build tools like `cabal`.

A command can be created using the `subparser` builder, and commands can be
added with the `command` modifier. For example

```haskell
subparser
  ( command "add" (info addOptions
      ( progDesc "Add a file to the repository" ))
 <> command "commit" (info commitOptions
      ( progDesc "Record changes to the repository" ))
)
```

Each command takes a full `ParserInfo` structure, which will be used to extract
a description for this command when generating a help text.

Note that all the parsers appearing in a command need to have the same type.
For this reason, it is often best to use a sum type which has the same
structure as the command itself. For example, for the parser above, you would
define a type like:

```haskell
data Options = Options
  { optGlobalOpt :: String
  , optGlobalFlag :: Bool
  ...
  , optCommand :: Command }

data Command
  = Add AddOptions
  | Commit CommitOptions
  ...
```

Alternatively, you can directly return an `IO` action from a parser, and
execute it using `join` from `Control.Monad`.

```haskell
start :: String -> IO ()
stop :: IO ()

opts :: Parser (IO ())
opts = subparser
  ( command "start" (info (start <$> argument str idm) idm)
 <> command "stop"  (info (pure stop) idm) )

main :: IO ()
main = join $ execParser (info opts idm)
```

## Option builders

Builders allow you to define parsers using a convenient combinator-based
syntax. Each builder takes a **modifier** as parameter, and returns a parser.

A modifier is a composition of functions which act on the option, setting
values for properties or adding features, and is used to build the option from
scratch and finally lift it to a single-option parser, which can then be
combined with other parsers using normal `Applicative` combinators.

Modifiers are instances of the `Monoid` typeclass, so they can be combined
using the composition function `mappend` (or simply `(<>)`).

See the [haddock documentation][builder-documentation] for `Options.Applicative.Builder`
for a full list of builders and modifiers.

## Advanced features

* [Bash completion]
* [Arrow interface]
* [Disambiguation]

 [Bash completion]: https://github.com/pcapriotti/optparse-applicative/wiki/Bash-Completion
 [Arrow interface]: https://github.com/pcapriotti/optparse-applicative/wiki/Arrows
 [Disambiguation]: https://github.com/pcapriotti/optparse-applicative/wiki/Disambiguation

## How it works

A `Parser a` is essentially a heterogeneous list of `Option`s, implemented with
existential types.

All options are therefore known statically (i.e. before parsing, not
necessarily before runtime), and can, for example, be traversed to generate a
help text.

See [this blog post][blog] for a more detailed explanation based on a
simplified implementation.

 [status-png]: https://api.travis-ci.org/pcapriotti/optparse-applicative.svg
 [status]: http://travis-ci.org/pcapriotti/optparse-applicative?branch=master
 [blog]: http://paolocapriotti.com/blog/2012/04/27/applicative-option-parser/
 [builder-documentation]: http://hackage.haskell.org/package/optparse-applicative/docs/Options-Applicative-Builder.html
 [hackage-png]: http://img.shields.io/hackage/v/optparse-applicative.svg
 [hackage]: http://hackage.haskell.org/package/optparse-applicative
