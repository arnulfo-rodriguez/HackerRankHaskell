module PasswordCracker(crackPasswords) where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import Data.Map (Map)
import Control.Applicative

type ChildNodes = Map Char PasswordsTrie
type IsWordEnd = Bool
data PasswordsTrie = RootNode ChildNodes | Node Char IsWordEnd ChildNodes deriving Show

  
addWord :: [Char] -> PasswordsTrie -> PasswordsTrie
addWord [lst] (RootNode nodes)  =
  case Map.lookup lst nodes of
    Nothing -> RootNode (Map.insert lst (Node lst True Map.empty) nodes)
    Just (Node _ _ cNodes) -> RootNode (Map.insert lst (Node lst True cNodes) nodes)

addWord [lst] (Node chr isWordEnd nodes)  =
  case Map.lookup lst nodes of
    Nothing -> Node chr isWordEnd (Map.insert lst (Node lst True Map.empty) nodes)
    Just (Node _ _ cNodes) -> Node chr isWordEnd (Map.insert lst (Node lst True cNodes) nodes)

addWord (first:remaining) (RootNode nodes)  =
  case Map.lookup first nodes of
    Nothing -> RootNode (Map.insert first (addWord remaining (Node first False Map.empty) ) nodes)
    Just node -> RootNode (Map.insert first (addWord remaining node ) nodes)

addWord (first:remaining) (Node c isWordEnd nodes)  =
  case Map.lookup first nodes of
    Nothing -> Node c isWordEnd (Map.insert first (addWord remaining (Node first False Map.empty) ) nodes)
    Just node -> Node c isWordEnd (Map.insert first (addWord remaining node ) nodes)

isInLanguage :: [Char] -> PasswordsTrie -> Maybe [[Char]]
isInLanguage theWord passwords =
   isInLanguageRec theWord "" passwords
  where
    isInLanguageRec :: [Char] -> [Char] -> PasswordsTrie -> Maybe [[Char]]
    isInLanguageRec [] _ _ = Nothing
    isInLanguageRec [lst] currentWord (RootNode nodes) =  case Map.lookup lst nodes of
                                              Just (Node _ True _) -> Just [currentWord ++ [lst]]
                                              _ -> Nothing

    isInLanguageRec [lst] currentWord (Node _ _ nodes) =  case Map.lookup lst nodes of
                                              Just (Node _ True _) -> Just [currentWord ++ [lst]]
                                              _ -> Nothing

    isInLanguageRec (first:rest) currentWord (RootNode nodes) = case Map.lookup first nodes of
                                                    Nothing -> Nothing
                                                    Just node@(Node _ True _) -> (((currentWord ++ [first]):) <$> isInLanguageRec rest "" passwords) <|> isInLanguageRec rest (currentWord ++ [first]) node
                                                    Just node -> isInLanguageRec rest (currentWord ++ [first]) node
    isInLanguageRec (first:rest) currentWord (Node _ _ nodes) = case Map.lookup first nodes of
                                                    Nothing -> Nothing
                                                    Just node@(Node _ True _) -> (((currentWord ++ [first]):) <$> isInLanguageRec rest "" passwords) <|> isInLanguageRec rest (currentWord ++ [first]) node 
                                                    Just node -> isInLanguageRec rest (currentWord ++ [first]) node

crackPasswords:: [Char] -> [[Char]] -> [Char]
crackPasswords password validPasswords =
  let
    trie = List.foldl (flip addWord) (RootNode Map.empty) validPasswords
    result =  isInLanguage password trie
  in Maybe.maybe "WRONG PASSWORD" List.unwords result