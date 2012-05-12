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

## How it works

A `Parser a` is essentially a heterogeneous list of `Option`s, implemented with
existential types.

All options are therefore known statically (i.e. before parsing, not
necessarily before runtime), and can, for example, be traversed to generate a
help text.

See [this blog post][blog] for a more detailed explanation based on a
simplified implementation.

 [blog]: http://www.soi.city.ac.uk/~ross/papers/Applicative.html
