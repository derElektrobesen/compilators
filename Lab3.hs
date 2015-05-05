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
filterRules :: [Rule] -> NonTerm -> [Rule]
filterRules (h:rules) nt
    | lRulePart h == nt = [h] ++ filterRules rules nt
    | otherwise = filterRules rules nt
filterRules [] _ = []

nextChoiceImpl :: [Rule] -> [Rule] -> [Term] -> CurrentConfiguration -> [CurrentConfiguration]
nextChoiceImpl (cur_rule:other_rules) all_rules terms (Configuration s pos fc (nt:sc)) =
    let local_conf = Configuration Q pos (fc ++ [Right (lRulePart cur_rule, ruleIndex cur_rule)]) ((rRulePart cur_rule) ++ sc)
        last_conf = Configuration s pos fc (nt:sc)
        new_conf = last_conf : createConfigurationImpl all_rules terms local_conf
        last_new_conf = last new_conf
        to_retry = Configuration B pos fc (nt:sc)
    in if state last_new_conf == B
        then new_conf ++ (nextChoiceImpl other_rules all_rules terms to_retry)
        else []                 -- success
nextChoiceImpl [] _ _ _ = []    -- no more choices

nextChoice :: [Rule] -> [Term] -> CurrentConfiguration -> [CurrentConfiguration]
nextChoice rules terms last_conf =
    nextChoiceImpl real_rules rules terms last_conf
    where
        (Right nt:_) = secondChain last_conf
        real_rules = filterRules rules nt

createConfigurationImpl :: [Rule] -> [Term] -> CurrentConfiguration -> [CurrentConfiguration]
createConfigurationImpl rules (t:terms) last_conf =
    case last_conf of
        Configuration Q pos _ ((Right non_term):sc) ->
            nextChoice rules (t:terms) last_conf
        Configuration Q pos fc ((Left term):sc) ->
            if term == t
                then last_conf : (createConfigurationImpl rules terms $ Configuration Q (pos + 1) (fc ++ [Left term]) sc)
                else last_conf : [Configuration B pos fc (secondChain last_conf)]
        Configuration Q pos fc [] -> last_conf : [Configuration T pos fc []]
createConfigurationImpl _ [] (Configuration _ pos fc []) = []
createConfigurationImpl _ [] last_conf =
    last_conf : [Configuration B ((position last_conf) - 1) (init $ firstChain last_conf) (last_sym : (secondChain last_conf))]
    where (Left t) = last $ firstChain last_conf
          last_sym = Left t

createConfiguration :: Grammar -> [Term] -> [CurrentConfiguration]
createConfiguration g c = createConfigurationImpl (ruleList g) c $ Configuration Q 0 [] [Right $ startSym g]
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
