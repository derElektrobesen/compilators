module Grammar where

import Data.List

-- data

data Term = Term Char
    deriving (Eq, Ord)
data NonTerm = NonTerm Char
    deriving (Eq, Ord)

type RPart = [Either Term NonTerm]
data Rule = Rule NonTerm RPart
    deriving (Eq, Ord)

rRulePart :: Rule -> RPart
rRulePart (Rule _ l) = l

lRulePart :: Rule -> NonTerm
lRulePart (Rule r _) = r

data Grammar = Grammar { nonTermList :: [NonTerm]
                       , termList :: [Term]
                       , ruleList :: [Rule]
                       , startSym :: NonTerm
                       } deriving (Eq, Ord)
emptyGrammar = Grammar { nonTermList = [], termList = [], ruleList = [], startSym = NonTerm '\0' }

instance Show Term where
    show (Term t) = [t]

instance Show NonTerm where
    show (NonTerm nt) = [nt]

showRPart :: RPart -> String
showRPart ((Left t):l) = show t ++ showRPart l
showRPart ((Right t):l) = show t ++ showRPart l
showRPart [] = ""

instance Show Rule where
    show (Rule l r) = show l ++ " -> " ++ showRPart r

instance Show Grammar where
    show g =
        "G = { " ++ (intercalate " " $ map show $ nonTermList g)
            ++ " } { " ++ (intercalate " " $ map show $ termList g)
            ++ " } P " ++ (show $ startSym g)
            ++ "\n" ++ (intercalate "\n" $ map show $ ruleList g)

-- parsing

parseNonTerms :: [String] -> [NonTerm] -> ([NonTerm], [String])
parseNonTerms (h:l) t
    | h == "}" = (t, l)
    | otherwise = parseNonTerms l $ t ++ [NonTerm $ head h]

parseTerms :: [String] -> [Term] -> ([Term], [String])
parseTerms (h:l) t
    | h == "}" = (t, l)
    | otherwise = parseTerms l $ t ++ [Term $ head h]

data ParseStep = ParseStart | NonTermsParsed | TermsParsed | RulesSignParsed
    deriving (Eq, Ord, Show)

parseGrammar :: ParseStep -> [String] -> Grammar -> Grammar
parseGrammar s (h:l) g
    | s == ParseStart && (h == "G" || h == "=") = parseGrammar s l g
    | s == ParseStart && h == "{" =
        let (nonTerms, list) = parseNonTerms l []
            new_g = Grammar nonTerms (termList g) (ruleList g) (startSym g)
        in parseGrammar NonTermsParsed list new_g
    | s == NonTermsParsed && h == "{" =
        let (terms, list) = parseTerms l []
            new_g = Grammar (nonTermList g) terms (ruleList g) (startSym g)
        in parseGrammar TermsParsed list new_g
    | s == TermsParsed = parseGrammar RulesSignParsed l g
    | s == RulesSignParsed = Grammar (nonTermList g) (termList g) (ruleList g) $ NonTerm $ head h

parseRPart :: String -> Grammar -> RPart -> RPart
parseRPart (h:l) g t
    | (NonTerm h) `elem` (nonTermList g) = parseRPart l g (t ++ [Right $ NonTerm h])
    | (Term h) `elem` (termList g) = parseRPart l g (t ++ [Left $ Term h])
parseRPart [] _ t = t

parseSingleRuleImpl :: [String] -> Grammar -> Rule
parseSingleRuleImpl (nt:delim:l:[]) g = Rule (NonTerm $ head nt) $ parseRPart l g []

parseSingleRule :: [String] -> Grammar -> Grammar
parseSingleRule l g = Grammar (nonTermList g) (termList g) ((ruleList g) ++ [parseSingleRuleImpl l g]) (startSym g)

parseRules :: [String] -> Grammar -> Grammar
parseRules (h:l) g = parseRules l $ parseSingleRule (words h) g
parseRules [] g = g

parseLines :: [String] -> Grammar
parseLines (g:l) =
    let grammar = parseGrammar ParseStart (words g) emptyGrammar
    in parseRules l grammar
