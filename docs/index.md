# optparse-applicative

optparse-applicative is a library for parsing command-line options.  It provides
a collection of primitive parsers, which can be assembled using an [applicative]
interface to build arbitrarily complex command-line option specifications.

optparse-applicative takes care of reading and validating the arguments passed
to the command line, handling and reporting errors, generating a usage line, a
help screen, and enabling context-sensitive bash completions.

**Table of Contents**

- [Introduction](#introduction)
    - [Basic parsers](#basic-parsers)
    - [Sequencing](#sequencing)
    - [Choice](#choice)
    - [Running parsers](#running-parsers)
- [Options](#options)
- [Builders](#builders)
    - [Regular options](#regular-options)
    - [Flags](#flags)
    - [Arguments](#arguments)
    - [Commands](#commands)
    - [Modifiers](#modifiers)
- [Custom parsing and error handling](#custom-parsing-and-error-handling)
    - [Parser runners](#parser-runners)
    - [Option readers](#option-readers)
    - [Preferences](#preferences)
    - [Disambiguation](#disambiguation)
    - [Displaying custom error messages](#displaying-custom-error-messages)
    - [Customising the help screen](#customising-the-help-screen)
- [Bash completion](#bash-completion)
- [Customizing completions](#customizing-completions)
    - [Internals](#internals)
- [Arrow interface](#arrow-interface)
- [FAQ](#faq)

## Introduction

The core type in optparse-applicative is `Parser`:

```haskell
data Parser a

instance Functor Parser
instance Applicative Parser
instance Alternative Parser
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

## Options

## Builders

### Regular options
### Flags
### Arguments
### Commands
### Modifiers

## Custom parsing and error handling

### Parser runners
### Option readers
### Preferences
### Disambiguation

It is possible to configure optparse-applicative to perform automatic
disambiguation of prefixes of long options. For example, given a program `foo`
with options `--filename` and `--filler`, typing

    $ foo --fil test.txt

fails, whereas typing

    $ foo --file test.txt

succeeds, and correctly identifies `"file"` as an unambiguous prefix of the
`filename` option.

Option disambiguation is *off* by default. To enable it, provide the
`disambiguate` modifier to the `prefs` builder, and pass the resulting
preferences to one of the parser runners that take an `ParserPrefs` parameter,
like `customExecParser`.

Here is a minimal example:

```haskell
import Options.Applicative

sample :: Parser ()
sample = () <$
  switch (long "filename") <*
  switch (long "filler")

main :: IO ()
main = customExecParser p opts
  where
    opts = info (helper <*> sample) idm
    p = prefs disambiguate

```

### Displaying custom error messages
### Customising the help screen

## Bash completion

`optparse-applicative` has built-in support for bash completion of command line
options and arguments. Any parser, when run using `execParser` (and similar
functions), is automatically extended with a few (hidden) options for bash
completion:

 - `--bash-completion-script`: this takes the full path of the program as
   argument, and prints a bash script, which, when sourced into a bash session,
   will install the necessary machinery to make bash completion work. For a
   quick test, you can run something like (for a program called `foo` on the
   `PATH`):

   ```console
   $ source <(foo --bash-completion-script `which foo`)
   ```

   Normally, the output of `--bash-completion-script` should be shipped with
   the program and copied to the appropriate directory (usually
   `/etc/bash_completion.d/`) during installation.

 - `--bash-completion-index`, `--bash-completion-word`: internal options used
   by the completion script to obtain a list of possible completions for a
   given command line.

## Customizing completions

By default, options and commands are always completed. So, for example, if the
program `foo` has an option with a long name `output`, typing

```console
$ foo --ou<TAB>
```

will complete `--output` automatically.

Arguments (either of regular options, or top-level) are not completed by
default. To enable completion for arguments, use one of the following modifiers
on a regular option or argument:

 - `completeWith`: specifies a list of possible completions to choose from;
 - `action`: specifies a completion "action". An action dynamically determines
   a list of possible completions. A full list of actions can be found in the
   [bash documentation];
 - `completer`: a completer is a function `String -> IO [String]`, returning
   all possible completions for a given string. You can use this modifier to
   specify a custom completion for an argument.

Completion modifiers can be used multiple times: the resulting completions will
call all of them and join the results.

### Internals

When running a parser with `execParser`, the parser is extended with
`bashCompletionParser`, which defines the above options.

When completion is triggered, the completion script calls the executable with
the special `--bash-completion-index` and `--bash-completion-word` options.

The original parser is therefore run in *completion mode*, i.e. `runParser` is
called on a different monad, which keeps track of the current state of the
parser, and exits when all arguments have been processed.

The completion monad returns, on failure, either the last state of the parser
(if no option could be matched), or the completer associated to an option (if
it failed while fetching the argument for that option).

From that we generate a list of possible completions, and print them to
standard output. They are then read by the completion script and put into the
`COMPREPLY` variable.

## Arrow interface

It is also possible to use the [Arrow syntax][arrows] to combine basic parsers.

This can be particularly useful when the structure holding parse results is
deeply nested, or when the order of fields differs from the order in which the
parsers should be applied.

Using functions from the `Options.Applicative.Arrows` module, one can write,
for example:

```haskell
data Options = Options
  { optArgs :: [String]
  , optVerbose :: Bool }

opts :: Parser Options
opts = runA $ proc () -> do
  verbosity <- asA (option (short 'v' <> value 0)) -< ()
  let verbose = verbosity > 0
  args <- asA (many (argument str idm)) -< ()
  returnA -< Options args verbose
```

where parsers are converted to arrows using `asA`, and the resulting composed
arrow is converted back to a `Parser` with `runA`.

See `tests/Examples/Cabal.hs` for a slightly more elaborate example using the
arrow syntax for defining parsers.

Note that the `Arrow` interface is provided only for convenience. The API based
on `Applicative` is just as expressive, although it might be cumbersome to use
in certain cases.

## FAQ

 [applicative]: http://hackage.haskell.org/package/base/docs/Control-Applicative.html
 [arrows]: http://www.haskell.org/arrows/syntax.html
 [attoparsec]: http://hackage.haskell.org/package/attoparsec
 [bash documentation]: http://www.gnu.org/software/bash/manual/html_node/Programmable-Completion-Builtins.html
 [monoid]: http://hackage.haskell.org/package/base/docs/Data-Monoid.html
 [parsec]: http://hackage.haskell.org/package/parsec
