It is possible to configure optparse-applicative to perform automatic disambiguation of prefixes of long options. For example, given a program `foo` with options `--filename` and `--filler`, typing

    $ foo --fil test.txt

fails, whereas typing

    $ foo --file test.txt

succeeds, and correctly identifies `"file"` as an unambiguous prefix of the `filename` option.

Option disambiguation is *off* by default. To enable it, provide the `disambiguate` modifier to the `prefs` builder, and pass the resulting preferences to one of the parser runners that take an `ParserPrefs` parameter, like `customExecParser`.

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