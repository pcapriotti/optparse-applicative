# optparse-applicative

optparse-applicative is a library for parsing command-line options.  It provides
a collection of primitive parsers, which can be assembled using an [applicative]
interface to build arbitrarily complex command-line option specifications.

optparse-applicative takes care of reading and validating the arguments passed
to the command line, handling and reporting errors, generating a usage line, a
help screen, and enabling context-sensitive bash completions.

**Table of Contents**

- [optparse-applicative](#optparse-applicative)
    - [Introduction](#introduction)
        - [Basic parsers](#basic-parsers)
        - [Sequencing](#sequencing)
        - [Choice](#choice)
        - [Running parsers](#running-parsers)

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

### Basic parsers

optparse-applicative provides a number of primitive parsers, usually
corresponding to single options, through its *Builder* interface.  Builders are
detailed in their [own section](#builder) of the manual, but for now, let's
just look at a few examples to get a feel for how parsers can be defined.

Here is a parser for a mandatory option with an argument:

```haskell
target :: Parser String
target = strOption
  (  long "hello"
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
  (  long "quiet"
  <> short 'q'
  <> help "Whether to be quiet" )
```

Here we used a `short` modifier to specify a one-letter name for the option.
This means that this switch can be set either with `--quiet` or `-q`.

Switches, unlike regular options, have no arguments. They simply return `True`
when found on the command line, and `False` otherwise.

Switches are special cases of *flags*, which can be created using `flag` (or
`flag'` for flags without a default value), and can be used to choose between
any two values of any type, rather than just `True` and `False`.

There are other kinds of basic parsers, and several ways to configure them.  We
will cover all of them in [Builders](#builders).

### Sequencing

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

No matter which parsers appear first in the sequence, options will still be
parsed in whatever order they appear in the command line. A parser with such a
property is sometimes called a *permutation parser*.

In our example, a command line like:

    --target world -q

will give the same result as

    -q --target world

Note, however, that the order of sequencing is still somewhat significant, in
that it affects the generated help text.

### Choice

It is quite common to find programs that can be configured in different ways
through the command line.  A typical example is a program that can be given a
text file as input, or alternatively read it directly from the standard input.

We can model this easily and effectively in Haskell using *sum types*:

```haskell
data Input
  = FileInput FilePath
  | StdInput

run :: Input -> IO ()
run = ...
```

However, we can't build a command line parser for the type `Input` using only
the primitive blocks introduced so far, plus sequencing.  We can certainly
define the two basic parsers involved:

```haskell
fileInput :: Parser Input
fileInput = FileInput <\$> strOption
  (  long "file"
  <> short 'f'
  <> metavar "FILENAME"
  <> help "Input file" )

stdInput :: Parser Input
stdInput = flag' StdInput
  (  long "stdin"
  <> help "Read from stdin" )
```

but we want to combine them in such a way that only one of them is ever
parsed, and never both; alas, the sequencing operator of `Applicative` doesn't
allow us to specify this kind of behaviour.

Fortunately, the `Parser` type constructor is also an instance of `Alternative`,
which provides a *choice* operator `(<|>)` for exactly this purpose:

```haskell
opts2 :: Parser Input
opts2 = fileInput <|> stdInput
```

Now `--file "foo.txt"` will be parsed as `FileInput "foo.txt"`, `--stdin` will
be parsed as `StdInput`, but a command line containing both options, like

    --file "foo.txt" --stdin

will be rejected.

### Running parsers

We have been discussing what parsers do, but so far, we haven't seen how to
actually have them process the command line and return a result, or report a
failure.

Before we can run a `Parser`, we need to wrap it into a `ParserInfo` structure,
that specifies a number of properties that only apply to top level parsers, such
as a header describing what the program does, to be displayed in the help
screen.

The function `info` will help with this step.  For the parser `opts` defined
above the corresponding `ParserInfo` could look something like:

```haskell
pinfo :: ParserInfo Options
pinfo = info (opts <**> helper)
  (  progDesc "Print a greeting for TARGET"
  <> header "hello - a demo of optparse-applicative" )
```

The `helper` parser that we added after `opts` just creates a dummy `--helper`
option that displays the help text.  Besides that, we just set some of the
fields of the `ParserInfo` structure with meaningful values.  They will be
displayed in the help text like so:

    hello - a test for optparse-applicative

    Usage: hello --hello TARGET [--quiet]
      Print a greeting for TARGET

    Available options:
      -h,--help                Show this help text
      --hello TARGET           Target for the greeting
      --quiet                  Whether to be quiet

Now that we have a `ParserInfo`, we can finally run the parser.  The simplest
way to do so is to simply call the `execParser` function in your `main`:

```haskell
main :: IO ()
main = do
  options <- execParser pinfo
  ...
```

The `execParser` function takes care of everything, including getting the
arguments from the command line, and displaying errors and help screens to the
user.

There are other ways to run a `ParserInfo`, in situations where you need finer
control over the behavior of your parser, or if you want to use it in pure
code. They will be covered in
[Custom parsing and error handling](#custom-parsing-and-error-handling).

 [applicative]: http://hackage.haskell.org/package/base/docs/Control-Applicative.html
 [monoid]: http://hackage.haskell.org/package/base/docs/Data-Monoid.html
 [parsec]: http://hackage.haskell.org/package/parsec
 [attoparsec]: http://hackage.haskell.org/package/attoparsec
