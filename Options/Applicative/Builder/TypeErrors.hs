{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
-- We don't want these orphan instances to
-- show up in the haddocks, they're ugly and
-- will only lead to confusion there.
{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Options.Applicative.Builder.TypeErrors () where

import GHC.TypeLits
import Options.Applicative.Builder.Internal

instance TypeError
  ('Text "Can't set names (with `short` or `long`) for `argument` builder." ':$$:
   'Text "Arguments are positional, and aren't selected on the command line with '-a' or '--arg'.")
         => HasName ArgumentFields where
   name = error "unreachable"

instance TypeError
  ('Text "Can't set names (with `short` or `long`) for `subparser` builder." ':$$:
   'Text "Subcommands are positional, and aren't selected on the command line with '-a' or '--arg'.")
         => HasName CommandFields where
   name = error "unreachable"

instance TypeError
  ('Text "Can't set completers for `flag` builders." ':$$:
   'Text "Flags don't have an argument which requires a custom completer." ':$$:
   'Text "Flags themselves are completed automatically by the completion system.")
         => HasCompleter FlagFields where
  modCompleter = error "unreachable"

instance TypeError
  ('Text "Can't set completers for `subparser` builders." ':$$:
   'Text "Subcommands are completed automatically by the completion system.")
         => HasCompleter CommandFields where
  modCompleter = error "unreachable"

instance TypeError
  ('Text "Can't set metavars for `flag` builders." ':$$:
   'Text "Flags don't have an argument which can be displayed as a metavar." ':$$:
   'Text "The long and short names will be displayed instead.")
         => HasMetavar FlagFields where
  hasMetavarDummy _ = error "unreachable"

instance TypeError
  ('Text "Can't set a default for `flag` builders with `value`." ':$$:
   'Text "If a default is required, use `flag` or `switch` instead of `flag'`")
         => HasValue FlagFields where
   hasValueDummy _ = error "unreachable"

instance TypeError
  ('Text "Can't set a default for `subparser` builders." ':$$:
   'Text "One can use an alternative if this behaviour is required.")
         => HasValue CommandFields where
   hasValueDummy _ = error "unreachable"
