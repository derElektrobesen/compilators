module Grammar_v2 where

import Data.List
import Data.List.Split
import Text.Regex
import Text.Regex.Posix

data Term = Term String
    deriving (Eq, Ord)
data NonTerm = NonTerm String
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
emptyGrammar = Grammar { nonTermList = [], termList = [], ruleList = [], startSym = NonTerm "" }

instance Show Term where
    show (Term t) = t

instance Show NonTerm where
    show (NonTerm nt) = nt

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

parseGrammar :: [String] -> Grammar
parseGrammar (non_terms:terms:first_rule:[]) =
    let non_terms_list = words non_terms
        terms_list = words terms
        (first:_) = words first_rule
    in Grammar
        { nonTermList = map NonTerm non_terms_list
        , termList = map Term terms_list
        , ruleList = []
        , startSym = NonTerm first
        }

parseRPart :: [String] -> Grammar -> RPart
parseRPart (atom:other) g
    | (NonTerm atom) `elem` (nonTermList g) = (Right $ NonTerm atom) : (parseRPart other g)
    | (Term atom) `elem` (termList g) = (Left $ Term atom) : (parseRPart other g)
    | otherwise = error $ "Invalid atom came: " ++ atom
parseRPart [] _ = []

createRule :: NonTerm -> [String] -> Grammar -> [Rule]
createRule l_part (head:tail) g =
    (Rule l_part (parseRPart (words head) g)) : (createRule l_part tail g)
createRule _ [] _ = []

parseRule :: [String] -> Grammar -> Grammar
parseRule (l_part:r_part:[]) g =
    let (l_rule_part:_) = words l_part
        r_rule_part = splitOn "|" r_part
    in Grammar
        { nonTermList = (nonTermList g)
        , termList = (termList g)
        , ruleList = (ruleList g) ++ (createRule (NonTerm l_rule_part) r_rule_part g)
        , startSym = (startSym g)
        }

parseWChain :: [String] -> Grammar -> [Term]
parseWChain (term:other) g
    | (Term term) `elem` (termList g) = (Term term) : (parseWChain other g)
    | otherwise = error $ "Invalid term in w chain: " ++ term
parseWChain [] _ = []

parseLines :: [String] -> Grammar -> (Grammar, Maybe [Term])
parseLines (line:tail) g
    | Just (_, _, _, a) <- matchRegexAll grammar_re line = parseLines tail (parseGrammar a)
    | Just (_, _, _, a) <- matchRegexAll rule_re line = parseLines tail (parseRule a g)
    | Just (_, _, _, (a:_)) <- matchRegexAll w_chain_re line = (g, Just $ parseWChain (words a) g)
    | otherwise = error $ "Invalid line in input: " ++ line
    where grammar_re = makeRegex "^G = {(.*)} {(.*)} P (.+)$"
          rule_re = makeRegex "^(.*) -> (.*)$"
          w_chain_re = makeRegex "^w = (.*)$"
parseLines [] g = (g, Nothing)
