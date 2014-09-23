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

 [arrows]: http://www.haskell.org/arrows/syntax.html