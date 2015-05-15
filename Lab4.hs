import System.Environment
import System.Directory
import Control.Monad
import Data.List
import Data.Either
import Data.Set (toList, fromList)
import Grammar_v2
import Debug.Trace

data Tree = EmptyTree | EmptyLeaf | Leaf Term | Node NonTerm [Tree]
    deriving (Eq)

instance Show Tree where
    show t =
        show_impl 0 t
        where
            show_str v depth =
                case () of
                    () -> replicate (2 * depth) ' ' ++ v ++ "\n"
            show_impl depth t =
                case t of
                    EmptyTree -> show_str "EmptyTree" depth
                    EmptyLeaf -> show_str "EmptyLeaf" depth
                    Leaf term -> show_str (show term) depth
                    Node nt trees -> show_str (show nt) depth ++ (intercalate "" $ map (show_impl (depth + 1)) trees)

produce :: Grammar -> [Term] -> [Rule] -> (Tree, [Term])
--produce _ terms rules | trace ("produce " ++ (show terms) ++ " " ++ (show rules)) False = undefined
produce _ income_terms ((Rule non_term []):[]) = ((Node non_term [EmptyLeaf]), income_terms)
produce g income_terms ((Rule lpart rpart):other_rules)
    | trees == [] || (last trees) == EmptyTree = produce g income_terms other_rules
    | otherwise = ((Node lpart trees), terms_list)
    where (trees, terms_list) = mkSubTree g income_terms income_terms rpart
produce g income_terms [] = (EmptyTree, income_terms)

mkSubTree :: Grammar -> [Term] -> [Term] -> RPart -> ([Tree], [Term])
--mkSubTree _ terms _ rpart | trace ("mkSubTree " ++ (show terms) ++ " " ++ (show rpart)) False = undefined
mkSubTree g income_terms (cur_term:other_terms) ((Left term):rule_tail)
    | term == cur_term = (((Leaf cur_term):tree_tail), new_terms)
    | otherwise = ([EmptyTree], income_terms)
    where (tree_tail, new_terms) = mkSubTree g other_terms other_terms rule_tail
mkSubTree g income_terms terms_chain ((Right non_term):rule_tail)
    | current_tree == EmptyTree = ([EmptyTree], income_terms)
    | otherwise = ((current_tree : other_tree), other_chain)
    where (current_tree, new_chain) = produce g terms_chain (filterRules (ruleList g) non_term)
          (other_tree, other_chain) = mkSubTree g new_chain new_chain rule_tail
mkSubTree _ _ terms_chain [] = ([], terms_chain)
mkSubTree _ income_terms [] _ = ([EmptyTree], income_terms)

mkTree :: Grammar -> [Term] -> Tree
mkTree g w_chain =
    let (tree, _) = produce g w_chain (filterRules (ruleList g) (startSym g)) in tree

main = do (fileName:_) <- getArgs
          fileExists <- doesFileExist fileName
          if fileExists
              then do contents <- readFile fileName
                      let content_lines = lines contents
                          (grammar, Just wchain) = parseLines content_lines emptyGrammar
                          tree = mkTree grammar wchain
                      putStrLn $ "w chain: " ++ (intercalate " " $ map show wchain)
                      putStrLn $ "Grammar: " ++ show grammar
                      putStrLn $ "Tree:\n" ++ show tree
              else do error "The file doesn't exist!"
