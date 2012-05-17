# Applicative option parser

This package contains utilities and combinators to define command line option
parsers.

## Getting started

Here is a simple example of an applicative option parser:

    data Sample = Sample
      { hello :: String
      , quiet :: Bool }

    sample :: Parser Sample
    sample = Sample
         <$> strOption
             ( long "hello"
             & metavar "TARGET"
             & help "Target for the greeting" )
         <*> switch
             ( long "quiet"
             & help "Whether to be quiet" )

The parser is built using [applicative style][applicative] starting from a set
of basic combinators. In this example, `hello` is defined as an option with a
`String` argument, while `quiet` is a boolean flag (called `switch`).

A parser can be used like this:

    greet :: Sample -> IO ()
    greet (Sample h True) = putStrLn $ "Hello, " ++ h
    greet _ = return ()

    main :: IO ()
    main = execParser opts >>= greet
      where
        opts = (info $ helper <*> sample)
          { infoFullDesc = True
          , infoProgDesc = "Print a greeting for TARGET"
          , infoHeader = "hello - a test for optparse-applicative" }

The `greet` function is the entry point of the program, while `opts` is a
complete description of the program, used when generating a help text. The
`helper` combinator takes any parser, and adds a `help` option to it (which
always fails).

The `hello` option in this example is mandatory (since it doesn't have a
default value), so running the program without any argument will display the
help text:

    hello - a test for optparse-applicative

    Usage: hello --hello TARGET [--quiet]
      Print a greeting for TARGET

    Common options:
      -h,--help                Show this help text
      --hello TARGET           Target for the greeting
      --quiet                  Whether to be quiet

containing a short usage summary, and a detailed list of options with
descriptions.

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

    strOption
    ( long "output"
    & short 'o'
    & metavar "FILE"
    & help "Write output to FILE" )

creates a regular option with a string argument (which can be referred to as
`FILE` in the help text and documentation), a long name "option" and a short
name "o". See below for more information on the builder syntax and modifiers.

A regular option can return an object of any type, provided you specify a
**reader** for it. A common reader is `auto`, used by the `option` builder,
which assumes a `Read` instance for the return type and uses it to parse its
argument. For example:

    lineCount :: Parser Int
    lineCount = option
                ( long "lines"
                & short 'n'
                & metavar "K"
                & help "Output the last K lines" )

specifies a regular option with an `Int` argument. We added an explicit type
annotation here, since without it the parser would have been polymorphic in the
output type. There's usually no need to add type annotations, however, because
the type will be normally inferred from the context in which the parser is
used.

You can also create a custom reader without using the `Read` typeclass, and set
it as the reader for an option using the `reader` modifier and the `nullOption`
builder:

    data FluxCapacitor = ...

    parseFluxCapacitor :: String -> Maybe FluxCapacitor

    option
    ( long "flux-capacitor"
    & reader parseFluxCapacitor )

### Flags

A **flag** is just like a regular option, but it doesn't take any arguments: it is
either present in the command line or not.

A flag has a default value and an **active value**. If the flag is found on the
command line, the active value is returned, otherwise the default value is
used. For example:

    data Verbosity = Normal | Verbose

    flag Normal Verbose
    ( long "verbose"
    & short 'v'
    & help "Enable verbose mode"

is a flag parser returning a `Verbosity` value.

Simple boolean flags can be specified using the `switch` builder, like so:

    switch
    ( long "keep-tmp-files"
    , help "Retain all intermediate temporary files" )

### Arguments

An **argument** parser specifies a positional command line argument.

The `argument` builder takes a reader parameter, and creates a parser which
will return the parsed value every time it is passed a command line argument
for which the reader succeeds. For example

    argument str ( metavar "FILE" )

creates an argument accepting any string.

Arguments are only displayed in the brief help text, so there's no need to
attach a description to them. They should manually documented in the program
description.

### Commands

A **command** can be used to specify a sub-parser to be used when a certain
string is encountered in the command line.

Commands are useful to implement command line programs with multiple functions,
each with its own set of options, and possibly some global options that apply
to all of them. Typical examples are version control systems like `git`, or
build tools like `cabal`.

A command can be created using the `subparser` builder, and commands can be
added with the `command` modifier. For example

    subparser
    ( command "add" (info addOptions)
        { infoProgDesc = "Add a file to the repository" }
    & command "commit") (info commitOptions)
        { infoProgDesc = "Record changes to the repository" }
    )

Each command takes a full `ParserInfo` structure, which will be used to extract
a description for this command when generating a help text.

Note that all the parsers appearing in a command need to have the same type.
For this reason, it is often best to use a sum type which has the same
structure as the command itself. For example, for the parser above, you would
define a type like:

    data Options = Options
      { globalOpt :: String
      , globalFlag :: Bool
      ...
      , commandOpts :: CommandOptions }

    data CommandOptions
      = AddOptions { ... }
      | CommitOptions { ... }
      ...

# Option builders

Builders allow you to define parsers using a convenient combinator-based
syntax. Each builder takes a **modifier** as parameter, and returns a parser.

A modifier is a composition of functions which act on the option, setting
values for properties or adding features, and is used to build the option from
scratch and finally lift it to a single-option parser, which can then be
combined with other parsers using normal `Applicative` combinators.

Modifiers are instances of the `Category` typeclass, so they can be combined
using the composition operator `(.)` from `Control.Category`, but the
`Options.Applicative.Builders` module provides a convenience operator `(&)`,
which is just a specialized version of flipped composition, so that you don't
need to import the `Category` module and hide the `(.)` operator from the
`Prelude`.

See the haddock documentation for `Options.Applicative.Builder` for a full list
of builders and modifiers.

## How it works

A `Parser a` is essentially a heterogeneous list of `Option`s, implemented with
existential types.

All options are therefore known statically (i.e. before parsing, not
necessarily before runtime), and can, for example, be traversed to generate a
help text.

See [this blog post][blog] for a more detailed explanation based on a
simplified implementation.

 [blog]: http://paolocapriotti.com/blog/2012/04/27/applicative-option-parser/
