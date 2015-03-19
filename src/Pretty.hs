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
    pbody body = color 7 (text ":-") <+> prettyBody body
    getLocals (Local v b) = let x = getLocals b
                             in (v : fst x, snd x)
    getLocals b = ([], b)
    (locals, body') = getLocals body
    prettyLocals = hsep $ map varText locals

prettyBody :: Body -> Doc
prettyBody (And a b) = prettyBody a <+> color 4 (char ',') <+> prettyBody b
prettyBody (Or a b) = prettyBody a <+> color 4 semi <+> prettyBody b
prettyBody (Check sym vars) = text sym <> parens (prettyVars vars)
prettyBody (Unify a b) = varText a <+> equals <+> varText b
prettyBody (LTrue) = color 5 (text "true")
prettyBody (LFalse) = color 5 (text "false")
prettyBody (Not b) = color 5 (text "\\+") <+> prettyBody b
prettyBody (Local v b) = empty
prettyBody (Extend c b) = braces (prettyClause c) <+> color 6 (text "=>") <+> prettyBody b

prettyVars :: [Variable] -> Doc
prettyVars = hsep . punctuate comma . map varText

color :: Int -> Doc -> Doc
color i d = text ("\ESC[" ++ show (30 + i) ++ "m") <> d <> text "\ESC[m"

varText :: Variable -> Doc
varText (Var a) = color 2 (text a)
varText (Atom a) = color 3 (text a)
