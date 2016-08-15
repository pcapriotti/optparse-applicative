## Version 0.13.0.0 (15 Aug 2016)

- Implement command groups, which allow subcommands to have their own
  usage description.

- Implement showHelpOnEmpty, which is similar to showHelpOnError, but only
  fires when a command or subcommand is begun, and suppresses the "Missing:"
  error text.

- Fix ghc 8.0 warnings

- Fix ghc 7.10 warnings

- Bump dependency bounds

- Add maybeReader function for convenient ReadM creation

- Move eitherReader to Readers section (for better discoverability)

- Fix hsubparser metavar override

- Remove ComplError, which was dead code.

- Reimplement Missing error generation, which overly complicated evalParser.

- Export Semigroup instances for types which are also Monoids. Removes
  mempty synonym `(<>)` export, as it clashes with Semigroup exports.
  One may need to import Data.Monoid or Data.Semigroup when upgrading.

- Use a Cabal test suite for tests, simplify test dependencies.

## Version 0.12.1.0 (18 Jan 2016)

- Updated dependency bounds.

- Improve subparser contexts to improve usage error texts

- Doc

- Fixed bugs
    * \# 164 - Invalid options and invalid arguments after parser has succeeded
               not displaying
    * \# 146 - multi-word filename completion is broken


## Version 0.12.0.0 (17 Sep 2015)

- Add "missing" error condition descriptions when required flags and arguments
  are not provided.

- Allow multiple short flags to be concatenated together behind a single
  hyphen, e.g. "-xcf".

- Updated dependency bounds on `process` and `ansi-wl-pprint`.

- Add `Show` and `Eq` instances to some types for easier debugging.

- Add defaultPrefs, a default preferences value

- Docs.

## Version 0.11.0.2 (17 Feb 2015)

- Updated dependency bounds.

## Version 0.11.0.1 (5 Oct 2014)

- Updated documentation.

## Version 0.11.0 (4 Oct 2014)

- Added Alternative instances for `Chunk` and `ReadM`.

- The `ReadM` monad is now a `ReaderT` for the argument being parsed.  User
  defined readers do not need to handle their argument explicitly, but can
  always access it using `readerAsk`.

- Argument builders now take a `ReadM` parameter, just like options.

- Fixed bugs
    * \#106 - argument should perhaps use `ReadM`

## Version 0.10.0 (1 Sep 2014)

- Parser execution and help text generation are now more modular, and allow for
  greater customisation.

- More consistent API for `option` and `argument` builders: now `option` takes
  a reader as argument, and `nullOption` is deprecated in favour of `option`.
  The `reader` modifier is gone.  Quick migration guide:

    * `option` (without a `reader` modifier) => `option auto`
    * `nullOption` (without a `reader` modifier) => `option disabled`
    * `option`/`nullOption` (with a `reader r` modifier) => `option r`.

- Added convenience builder `strArgument`, equivalent to `argument str`.

- Removed functions deprecated from at least version 0.8.0.

- Switched test infrastructure to `tasty`.

- Fixed bugs
    * \#63 - Inconsistency between 'argument' and 'strOption' types

## Version 0.9.1.1 (31 Jul 2014)

- Fixed bugs
    * \#97 - Version 0.9.1 fails test suite

## Version 0.9.1 (30 Jul 2014)

- Documentation tweaks.

- Added low-level function to handle parse results (pull request \#94).

- `ParserResult` now has a `Show` instance (see issue \#95).

- Fixed bugs
    * \#93 - Formatting problem for several sub-parsers

## Version 0.9.0 (23 May 2014)

- The option returned by `abortOption` is now visible by default.

## Version 0.8.1 (5 May 2014)

- Fixed bugs
    * \#74 - Missing newline

## Version 0.8.0.1 (19 Mar 2014)

- Fixed bugs
    * \#73 - Release 0.8.0 is broken

## Version 0.8.0 (16 Mar 2014)

- Help page formatting.  Added `columns` preference modifier,
  which can be used to specify the number of columns in the output
  terminal.

- Deprecated `arguments` and `arguments1` builders. Using `many` and `some` on a
  parser built using `argument` now returns a multiple argument parsers that
  behaves correctly with respect to `--`.

- Fixed bugs
    * \#60 - runParser can't be called
    * \#64 - --help behaviour

## Version 0.7.0.2 (18 Oct 2013)

- Fixed bugs
    * \#51 - Build fails with ghc 6.12.3 and ghc 7.0.4

## Version 0.7.0.1 (18 Oct 2013)

- Minor docs fixes

## Version 0.7.0 (17 Oct 2013)

- Added builders for options that always fail. This makes it
  easier to create options that just print an error message or
  display some brief information and then exit (like `--version`).

- Added `execParserMaybe` and `customExecParserMaybe` functions
  (pull request #49).

- Fixed bugs
    * \#47 - Current master prints help text instead of error
    * \#48 - Can we have an eitherReader convenience function?
    * \#50 - In order parsing problems.
    * \#22 - Strict (no-intersperse) arguments

## Version 0.6.0 (11 Oct 2013)

- Arguments are now always parsed in order.

- Fixed bugs
    * \#40 - Add context information to error messages
    * \#41 - Readme uses old reader API
    * \#38 - Internal types leaking into public API
    * \#44 - Can the build input restriction process == 1.1.* be relaxed?
    * \#28 - Help for subcommands

## Version 0.5.2.1 (24 Dic 2012)

- Minor docs fixes.

## Version 0.5.2 (23 Dic 2012)

- Fixed compatibility with GHC 7.2.

## Version 0.5.1 (23 Dic 2012)

- There is a new parser preference `noBacktrack`, that controls whether how a
  failure in a subparser is propagated. By default, an unknown option in a
  subparser causes the option to be looked up in parent parsers. When
  `noBacktrack` is used, this behavior is disabled. This is useful to implement
  subcommands that have no relations with their parent commands.

- Fixed bugs
    * \#35 - Artifacts of "hidden"
    * \#31 - Backtracking on commands
    * \#25 - Allow for using Maybe in options types to specify optional arguments
    * \#34 - No simple/obvious way to add a --version switch
    * \#29 - Document Mod
    * \#26 - Improve docs for the `Arrow` interface

## Version 0.5.0 (22 Dic 2012)

- Fewer GHC extensions required.

- Improved error handling: unrecognized options now result in an error message.

- By default, the full help text is not displayed on parse errors anymore.
  This behavior can be controlled with the `prefShowHelpOnError` field of
  `ParserPrefs`.

- The `(&)` operator is now deprecated. Modifiers can still be combined using
  `(<>)` or `mappend`.

- Fixed bugs
    * \#37 - Use (\<\>) instead of (&) in documentation

## Version 0.4.3 (09 Dic 2012)

- Updated dependency bounds.

## Version 0.4.2 (26 Nov 2012)

- Fixed bugs
    * \#27 - Please include the test source files in the cabal sdist tarball

## Version 0.4.1 (04 Sep 2012)

- Fixed bugs
    * \#19 - Regression

## Version 0.4.0 (05 Aug 2012)

- Brief help text for nested commands now shows the full command line.

- Fixed inefficiency in the `arguments` parsers for long argument lists.

- Added automatic [bash
completion](https://github.com/pcapriotti/optparse-applicative/wiki/Bash-Completion).

- Added `disambiguate` modifier for `prefs`, which enabled automatic
disambiguation of option abbreviations. With disambiguation on, a command line
like:

        foo --out

    will match an option called `--output`, as long as its the only one starting
    with the string `out`.

- Added `briefDesc` modifier.

- Fixed bugs
    * \#8 - Long options not disambiguated
    * \#10 - Shell completions
    * \#16 - Possible memory leak?

## Version 0.3.2 (31 Jul 2012)

- Fixed bug where both branches of an alternative could be matched.

- Improved brief help text for alternatives.

## Version 0.3.1 (30 Jul 2012)

- Added new `showDefault` and `showDefaultWith` modifiers, which will result in
the default value (if present) to be displayed in the help text.

- Fixed bugs
    * \#12 - Optionally display default values in help

## Version 0.3.0 (30 Jul 2012)

- Option modifiers are now instances of `Monoid` instead of `Category`.

- Dropped dependencies on data-default and data-lens.

- Fixed bugs
    * \#14 - "arguments" can no longer take a list as a default

## Version 0.2.0 (23 Jul 2012)

- Parser is now an instance of Alternative. This makes it possible to build
certain complex parsers that were not definable before. See
`tests/Examples/Alternatives.hs` for a simple example.

- Removed `multi` modifier. You can now use the `many` or `some` methods from
`Alternative`, instead, to create parsers for options that can appear more than
once.

- Added new `flag'` builder that returns a flag without a default value.
Although flags without default values were not useful before, with the addition
of `Alternative` combinators, they do have valid use cases.

- Added new `internal` modifier for options. An internal option is completely
invisible in the help text.

- Added a new `customExecParser` function, which takes an additional
`ParserPrefs` parameter. At the moment, `ParserPrefs` can only be used to
control how many-valued option metavars are displayed in the help text. Setting
its `multiSuffix` field to e.g. `...` will result in an `arguments` parser
description like `[METAVAR]...`.

- Fixed bugs
    * \#6 - "arguments" swallows options
    * \#5 - Help formatting for "arguments" misleading

## Version 0.1.1 (21 Jul 2012)

- New arrow interface

- Fixed bugs
      * \#7 - "arguments" reads positional arguments in reverse

## Version 0.1.0 (07 Jul 2012)

- Improved error reporting internals

- Removed template-haskell dependency

- Fixed bugs:
      * \#3 - No help for subparsers
      * \#4 - Extra empty lines around command list

## Version 0.0.1 (09 Jun 2012)

- Initial release.
