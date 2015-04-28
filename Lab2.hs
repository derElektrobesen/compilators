import System.Environment
import System.Directory
import System.IO
import Control.Monad
import Data.List
import Data.Maybe
import Data.Either
import Data.String
import Data.Set (toList, fromList)

-- data

data Term = Term Char
    deriving (Eq, Ord, Show)
data NonTerm = NonTerm Char
    deriving (Eq, Ord, Show)

type RPart = [Either Term NonTerm]
data Rule = Rule NonTerm RPart
    deriving (Eq, Ord, Show)

rRulePart :: Rule -> RPart
rRulePart (Rule _ l) = l

lRulePart :: Rule -> NonTerm
lRulePart (Rule r _) = r

data Grammar = Grammar { nonTermList :: [NonTerm]
                       , termList :: [Term]
                       , ruleList :: [Rule]
                       , startSym :: NonTerm
                       } deriving (Eq, Ord, Show)
emptyGrammar = Grammar { nonTermList = [], termList = [], ruleList = [], startSym = NonTerm '\0' }

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

-- algorithms

filterRulesImpl :: [Rule] -> (Rule -> Bool) -> [Rule] -> [Rule]
filterRulesImpl (h:rules) f already_filtered
    | f h       = filterRulesImpl rules f (already_filtered ++ [h])
    | otherwise = filterRulesImpl rules f already_filtered
filterRulesImpl [] _ r = r

filterRules :: (Rule -> Bool) -> [Rule] -> [Rule]
filterRules f r = filterRulesImpl r f []

emptyLangFilterNT :: [NonTerm] -> NonTerm -> Bool
emptyLangFilterNT (nt:list) non_term
    | nt == non_term = True
    | otherwise      = emptyLangFilterNT list non_term
emptyLangFilterNT [] _ = False

emptyLangFilter :: [Term] -> [NonTerm] -> RPart -> Bool
emptyLangFilter terms non_terms (t:not_processed) =
    case t of
        Left term       -> next
        Right non_term  -> if emptyLangFilterNT non_terms non_term  then next else False
    where next = emptyLangFilter terms non_terms not_processed
emptyLangFilter _ _ [] = True

findNiChainImpl :: Grammar -> [NonTerm] -> [Rule]
findNiChainImpl g previous_chain = filterRules (\rule -> emptyLangFilter (termList g) previous_chain (rRulePart rule)) $ ruleList g

findNiChain :: Grammar -> [NonTerm] -> [NonTerm]
findNiChain g previous_chain =
    let new_chain = findNiChainImpl g previous_chain
    in toList $ fromList $ map (\rule -> lRulePart rule) new_chain

findNeChainImpl :: Grammar -> [NonTerm] -> [NonTerm]
findNeChainImpl g previous_chain =
    let new_chain = findNiChain g previous_chain
    in if new_chain == previous_chain then new_chain else findNeChainImpl g new_chain

findNeChain :: Grammar -> [NonTerm]
findNeChain g = findNeChainImpl g []

isLanguageEmpty :: Grammar -> Bool
isLanguageEmpty g =
    let ne_chain = findNeChain g
    in if (startSym g) `elem` ne_chain then True else False

-- main

main = do (fileName:_) <- getArgs
          fileExists <- doesFileExist fileName
          if fileExists
              then do contents <- readFile fileName
                      let grammar = parseLines $ lines contents
                          ne_chain = findNeChain grammar
                      putStrLn $ show grammar
                      putStrLn $ show ne_chain
              else do putStrLn "The file doesn't exist!"
