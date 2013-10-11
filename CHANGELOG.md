## Version 0.6.0 (11 Oct 2013)

- Arguments are now always parsed in order.

- Fixed bugs
    * \#40 - Add context information to error messages
    * \#41 - Readme uses old reader API
    * \#38 - Internal types leaking into public API
    * \#44 - Can the build input restriction process == 1.1.* be relaxed?
    * \#28 - Help for subcommands

## Version 0.5.2.1 (24 Dic 2012)

- Minor docs fixes

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

- Fewer GHC extensions required

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
