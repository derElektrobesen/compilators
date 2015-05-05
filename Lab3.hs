import System.Environment
import System.Directory
import Control.Monad
import Data.List
import Data.Either
import Data.Set (toList, fromList)
import Grammar

type L1Chain = [Either Term (NonTerm, Integer)]
type L2Chain = [Either Term NonTerm]

showL1Chain :: L1Chain -> String
showL1Chain [] = "e"
showL1Chain c  = showL1ChainImpl c

showL2Chain :: L2Chain -> String
showL2Chain ((Left t):l) = show t ++ showL2Chain l
showL2Chain ((Right t):l) = show t ++ showL2Chain l
showL2Chain [] = "$"

showL1ChainImpl :: L1Chain -> String
showL1ChainImpl ((Left t):l) = show t ++ showL1ChainImpl l
showL1ChainImpl ((Right t):l) = (show $ fst t) ++ (show $ snd t) ++ showL1ChainImpl l
showL1ChainImpl [] = ""

data ConfigurationState = Q | B | T
    deriving (Eq, Ord)
data CurrentConfiguration = Configuration { state :: ConfigurationState
                                          , position :: Integer
                                          , firstChain :: L1Chain
                                          , secondChain :: L2Chain
                                          } deriving (Eq, Ord)

instance Show ConfigurationState where
    show Q = "q"
    show B = "b"
    show T = "t"

instance Show CurrentConfiguration where
    show conf = "( " ++ (show $ state conf) ++ ", "
                ++ (show $ position conf) ++ ", "
                ++ (showL1Chain $ firstChain conf) ++ ", "
                ++ (showL2Chain $ secondChain conf) ++ " )"

-- parsing

parseWChainImpl :: Grammar -> String -> [Term] -> [Term]
parseWChainImpl g (h:l) c
    | (Term h) `elem` (termList g) = parseWChainImpl g l (c ++ [Term h])
    | otherwise = error $ "Invalid term found in w chain: " ++ show h
parseWChainImpl _ [] c = c

parseWChain :: Grammar -> String -> [Term]
parseWChain g ('w':' ':'=':' ':l) = parseWChainImpl g l []
parseWChain _ _ = error "Invalid w chain format"

-- algorithms

applyChoice :: [Rule] -> [Rule] -> [Term] -> CurrentConfiguration -> [CurrentConfiguration]
applyChoice (cur_rule:other_rules) all_rules terms (Configuration Q pos fc ((Right atom):sc)) =
    let conf = Configuration Q pos fc ((Right atom):sc)
        r = conf : grow all_rules terms (Configuration Q pos (fc ++ [Right (lRulePart cur_rule, ruleIndex cur_rule)]) ((rRulePart cur_rule) ++ sc))
        last_conf = last r
    in if state (last r) == T
        then r
        else r ++ applyChoice other_rules all_rules terms conf
applyChoice [] _ _ conf =
    [conf]

grow :: [Rule] -> [Term] -> CurrentConfiguration -> [CurrentConfiguration]
grow rules_list terms (Configuration Q pos fc ((Right atom):sc)) =
     applyChoice filtered rules_list terms (Configuration Q pos fc ((Right atom) : sc))
     where filtered = filterRules rules_list atom
grow rules_list (head_term:other_terms) (Configuration Q pos fc ((Left atom):sc))
     | head_term == atom = passed : grow rules_list other_terms (Configuration Q (pos + 1) (fc ++ [Left atom]) sc)
     | otherwise = passed : [Configuration B pos fc ((Left atom):sc)]
     where passed = (Configuration Q pos fc ((Left atom):sc))
grow rules_list [] (Configuration Q pos fc []) =
     [(Configuration Q pos fc []), (Configuration T pos fc [])]
grow rules_list [] (Configuration Q pos fc sc) =
     [(Configuration Q pos fc sc), (Configuration B pos fc sc)]

filterRules :: [Rule] -> NonTerm -> [Rule]
filterRules (h:rules) nt
    | lRulePart h == nt = [h] ++ filterRules rules nt
    | otherwise = filterRules rules nt
filterRules [] _ = []

createConfiguration :: Grammar -> [Term] -> [CurrentConfiguration]
createConfiguration g c = grow (ruleList g) c $ Configuration Q 0 [] [Right $ startSym g]
-- main

main = do (fileName:_) <- getArgs
          fileExists <- doesFileExist fileName
          if fileExists
              then do contents <- readFile fileName
                      let content_lines = lines contents
                          grammar = parseLines $ init content_lines
                          wchain = parseWChain grammar $ last content_lines
                          conf = createConfiguration grammar wchain
                      putStrLn $ "w chain: " ++ (intercalate " " $ map show wchain)
                      putStrLn $ "Grammar: " ++ show grammar
                      putStrLn $ "\nConfiguration: \n" ++ (intercalate "\n" $ map show conf)
              else do error "The file doesn't exist!"
