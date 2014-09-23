## Introduction

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

 [bash documentation]: http://www.gnu.org/software/bash/manual/html_node/Programmable-Completion-Builtins.html

Completion modifiers can be used multiple times: the resulting completions will
call all of them and join the results.

## How it works

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
