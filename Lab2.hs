import System.Environment
import System.Directory
import Control.Monad
import Data.List
import Data.Either
import Data.Set (toList, fromList)
import Grammar

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
