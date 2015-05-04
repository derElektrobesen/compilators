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

-- algorithms

filterRulesImpl :: [Rule] -> (Rule -> Bool) -> [Rule] -> [Rule]
filterRulesImpl (h:rules) f already_filtered
    | f h       = filterRulesImpl rules f (already_filtered ++ [h])
    | otherwise = filterRulesImpl rules f already_filtered
filterRulesImpl [] _ r = r

filterRules :: (Rule -> Bool) -> [Rule] -> [Rule]
filterRules f r = filterRulesImpl r f []

emptyLangFilter :: [Term] -> [NonTerm] -> RPart -> Bool
emptyLangFilter terms non_terms (t:not_processed) =
    case t of
        Left term       -> next
        Right non_term  -> if non_term `elem` non_terms then next else False
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

createRuleListFromNeChain :: Grammar -> [NonTerm] -> [Rule]
createRuleListFromNeChain g ne_chain = filterRules (\rule -> emptyLangFilter (termList g) ne_chain (rRulePart rule)) $ ruleList g

createGrammarFromNeChain :: Grammar -> [NonTerm] -> Grammar
createGrammarFromNeChain g ne_chain = Grammar ne_chain (termList g) (createRuleListFromNeChain g ne_chain) (startSym g)

findViChain :: [Rule] -> RPart -> RPart
findViChain rules prev_v =
    let new_rules = filterRules (\rule -> (Right $ lRulePart rule) `elem` prev_v) rules
    in toList $ fromList $ intercalate [] $ map (\r -> (Right $ lRulePart r):(rRulePart r)) new_rules

removeUnreachableSymbolsImpl :: Grammar -> RPart -> RPart
removeUnreachableSymbolsImpl g cur_chain =
    let new_chain = findViChain (ruleList g) cur_chain
    in if new_chain == cur_chain then new_chain else removeUnreachableSymbolsImpl g new_chain

unreachableRulesFilter :: RPart -> Rule -> Bool
unreachableRulesFilter chain (Rule t l) = (Right t) `elem` chain && (all (\e -> e `elem` chain) l)

removeUnreachableSymbols :: Grammar -> Grammar
removeUnreachableSymbols g =
    let just_chain = removeUnreachableSymbolsImpl g [Right $ startSym g]
    in Grammar (rights just_chain) (lefts just_chain) (filterRules (unreachableRulesFilter just_chain) (ruleList g)) (startSym g)

-- main

main = do (fileName:_) <- getArgs
          fileExists <- doesFileExist fileName
          if fileExists
              then do contents <- readFile fileName
                      let grammar = parseLines $ lines contents
                          ne_chain = findNeChain grammar
                          new_grammar = createGrammarFromNeChain grammar ne_chain
                          new_new_grammar = removeUnreachableSymbols new_grammar
                      putStrLn $ "Default grammar:\n" ++ show grammar
                      putStrLn $ "\nNe chain: " ++ show ne_chain
                      putStrLn $ "\nWithout useless symbols:\n" ++ show new_new_grammar
              else do putStrLn "The file doesn't exist!"
