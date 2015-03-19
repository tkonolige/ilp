module Pretty (pretty) where

import Text.PrettyPrint
import ILP

pretty :: Clause -> String
pretty c = render pp -- TODO: get terminal width
  where pp = prettyClause c <> text "."

prettyClause :: Clause -> Doc
prettyClause (Clause sym vars body) = text sym <> parens (prettyVars vars) <+> prettyLocals <+> pbody body'
  where
    pbody LTrue = empty
    pbody body = text ":-" <+> prettyBody body
    getLocals (Local v b) = let x = getLocals b
                             in (v : fst x, snd x)
    getLocals b = ([], b)
    (locals, body') = getLocals body
    prettyLocals = hsep $ map varText locals
    -- prettyLocals = foldr (\v d -> text " " <> varText v <> d) empty locals

prettyBody :: Body -> Doc
prettyBody (And a b) = prettyBody a <+> char ',' <+> prettyBody b
prettyBody (Or a b) = prettyBody a <+> semi <+> prettyBody b
prettyBody (Check sym vars) = text sym <> parens (prettyVars vars)
prettyBody (Unify a b) = varText a <+> equals <+> varText b
prettyBody (LTrue) = text "true"
prettyBody (LFalse) = text "false"
prettyBody (Not b) = text "\\+" <+> prettyBody b
prettyBody (Local v b) = empty
prettyBody (Extend c b) = braces (prettyClause c) <+> text "=>" <+> prettyBody b

prettyVars :: [Variable] -> Doc
prettyVars = hsep . punctuate comma . map varText

varText :: Variable -> Doc
varText (Var a) = text a
varText (Atom a) = text a
