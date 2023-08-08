module Options.Applicative.Help.HelpDoc
  ( HelpDoc,
    HelpType (..),
    ansiDocToHelpDoc,
    helpDocToAnsiDoc,
    annotateHelp,
    annotateStyle,
  )
where

import Options.Applicative.Help.Pretty (AnsiDoc, AnsiStyle, annotate, reAnnotate)
import qualified Prettyprinter as PP
import Prelude

type HelpDoc = PP.Doc HelpAnn

data HelpAnn = HelpAnnType HelpType | HelpAnnStyle AnsiStyle

data HelpType = CmdName | OptionName | Description | Title | Metavar

annotateHelp :: HelpType -> HelpDoc -> HelpDoc
annotateHelp helpType = annotate $ HelpAnnType helpType

annotateStyle :: AnsiStyle -> HelpDoc -> HelpDoc
annotateStyle ansiStyle = annotate $ HelpAnnStyle ansiStyle

ansiDocToHelpDoc :: AnsiDoc -> HelpDoc
ansiDocToHelpDoc = reAnnotate HelpAnnStyle

helpDocToAnsiDoc :: HelpDoc -> AnsiDoc
-- TODO(Martin): I will want to probably use reAnnotate here -> for each HelpAnn, I will generate 0
-- to N AnsiStyle annotations. However maybe I should not do this for Docs, but for SimpleDocStream,
-- as they recommended! So maybe we should not implement this function, but instead one that does
-- SimpleDocStream HelpAnn -> SimpleDocStream AnsiStyle.
helpDocToAnsiDoc = error "TODO"
