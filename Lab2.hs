import System.Environment
import System.Directory
import System.IO
import Control.Monad
import Data.List
import Data.Maybe
import Data.Either
import Data.String

data Term = Term Char
    deriving (Eq, Ord, Show)
data NonTerm = NonTerm Char
    deriving (Eq, Ord, Show)

data Rule = Rule NonTerm [Either Term NonTerm]
    deriving (Eq, Ord, Show)

data Grammar = Grammar { nonTermList :: [NonTerm]
                       , termList :: [Term]
                       , ruleList :: [Rule]
                       , startSym :: NonTerm
                       } deriving (Eq, Ord, Show)
emptyGrammar = Grammar { nonTermList = [], termList = [], ruleList = [], startSym = NonTerm '\0' }

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

parseRPart :: String -> Grammar -> [Either Term NonTerm] -> [Either Term NonTerm]
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

main = do (fileName:_) <- getArgs
          fileExists <- doesFileExist fileName
          if fileExists
              then do contents <- readFile fileName
                      let rules = parseLines $ lines contents
                      putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"
                      putStrLn $ show rules
              else do putStrLn "The file doesn't exist!"
