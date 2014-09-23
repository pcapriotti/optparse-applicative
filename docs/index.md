# optparse-applicative

optparse-applicative is a library for parsing command-line options.  It provides
a collection of primitive parsers, which can be assembled using an [applicative]
interface to build arbitrarily complex command-line option specifications.

optparse-applicative takes care of reading and validating the arguments passed
to the command line, handling and reporting errors, generating a usage line, a
help screen, and enabling context-sensitive bash completions.

## Introduction

The core type in optparse-applicative is `Parser`:

```haskell
data Parser a

instance Functor Parser
instance Applicative Parser
```

A value of type `Parser a` represents a specification for a set of options,
which, if parsed correctly, will eventually result in a value of type `a`.

If you are familiar with parser combinator libraries like [parsec] or
[attoparsec], you will feel right at home with optparse-applicative.

If not, don't worry: all you really need to learn are a few basic parsers, and
the two operations of *sequencing* and *choice*, provided by the `Applicative`
and `Alternative` type classes respectively.

## Basic parsers

optparse-applicative provides a number of primitive parsers, usually
corresponding to single options, through its *Builder* interface.  Builders are
detailed in their [own section](builders.md) of the manual, but for now, let's
just look at a few examples to get a feel for how parsers can be defined.

Here is a parser for a mandatory option with an argument:

```haskell
target :: Parser String
target = strOption
       ( long "hello"
      <> metavar "TARGET"
      <> help "Target for the greeting" )
```

You can see that we are defining an option parser for a string argument, with
*long* option name "hello", *metavariable* "TARGET", and the given help text.
This means that the `target` parser defined above will require an option like

    --target world

on the command line. The metavariable and the help text will appear in the
generated help text, but don't otherwise affect the behaviour of the parser.

The attributes passed to the option are called *modifiers*, and are composed
using the [monoid] operation `(<>)`.

Options with an argument such as `target` are referred to as *regular options*,
and are by far the most common.  Another example of option is a boolean
*switch*:

```haskell
quiet :: Parser Bool
quiet = switch
      ( long "quiet"
     <> short 'q'
     <> help "Whether to be quiet" )
```

Here we used a `short` modifier to specify a one-letter name for the option.
This means that this switch can be set either with:

    --quiet

or with

    -q

Switches, unlike regular options, have no arguments. They simply return `True`
when found on the command line, and `False` otherwise.

## Sequencing

We now want to combine `target` and `quiet` into a single parser that accepts
both options and returns a combined value.  So let's begin by defining the type
of the result:

```haskell
data Options = Options
  { optTarget :: String
  , optQuiet :: Bool }
```

and now it's just a matter of using `Applicative`'s sequencing operator `(<*>)`
to combine the two previously defined parsers:

```haskell
opts :: Parser Options
opts = Options <$> target <*> quiet
```



 [applicative]: http://hackage.haskell.org/package/base/docs/Control-Applicative.html
 [monoid]: http://hackage.haskell.org/package/base/docs/Data-Monoid.html
 [parsec]: http://hackage.haskell.org/package/parsec
 [attoparsec]: http://hackage.haskell.org/package/attoparsec
