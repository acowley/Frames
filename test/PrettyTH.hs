{-# LANGUAGE OverloadedStrings #-}
module PrettyTH where
import Data.Char (isSpace)
import Frames
import Language.Haskell.TH
import Language.Haskell.TH.PprLib
import Text.PrettyPrint
import Text.Regex.Applicative

import Temp

generateCode :: String -> String -> Q Exp
generateCode rowName txt =
    withTempContents
      txt
      (\fp -> tableTypes rowName fp
              >>= stringE
              . makePretty
              . renderStyle (style {ribbonsPerLine=1.0, lineLength=150})
              . to_HPJ_Doc . ppr)

-- | Make template haskell-generated code more readable by
-- unqualifying common names, making type operators infix, erasing
-- inferrable types, and adding a bit of whitespace.
makePretty :: String -> String
makePretty = -- Add new lines before type synonym definitions
             replace' "\ntype " "\n\ntype "
             -- Make :-> and ': type operators infix
             . (!! 10) . iterate (replace infixCons)
             . replace infixNil
             . replace infixCol
               -- Erase inferrable type
             . replace ((\x y -> x ++ " ∈ " ++ y)
                        <$> ("RElem " *> some (psym (not . isSpace)))
                        <*> ((some (psym isSpace) *> some (psym (not . isSpace)))
                             <* " (RIndex " <* some (psym (/= ')')) <* ")"))
               -- Unqualify names
             . replace' "Frames.CSV.ParserOptions" "ParserOptions"
             . replace' "GHC.Base." ""
             . replace' "GHC.Types." ""
             . replace' "Data.Vinyl.Core." ""
             . replace' "Data.Vinyl.Lens." ""
             . replace' "Data.Vinyl.TypeLevel." ""
             . replace' "Frames.Rec." ""
             . replace' "Frames.RecLens." ""
             . replace' "Data.Proxy." ""
             . replace' "Data.Text." "T."
             . replace' "Data.Text.Internal." "T."
             . replace' "'GHC.Types.:" "(':)"
  where replace' orig replacement = replace (replacement <$ string orig)
        infixCol = (\x y -> '"' : x ++ "\" :-> " ++ y)
                   <$> ("Frames.Col.:-> \"" *> (some (psym (/= '"'))) <* "\"" 
                        <* some (psym isSpace))
                   <*> some (psym (/= ' '))
        infixNil = (\x -> '[' : x ++ "]")
                   <$> ("((':) (" *> some (psym (/= ')')) <* ")" 
                        <* some (psym isSpace) <* "'[])")
        infixCons = (\x y -> '[' : x ++ ", " ++ y ++ "]")
                    <$> ("((':) (" *> some (psym (/= ')')) <* ")"
                         <* some (psym isSpace) <* "[")
                    <*> (some (psym (/= ']')) <* "])")
