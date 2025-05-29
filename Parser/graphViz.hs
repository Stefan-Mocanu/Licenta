{-# LANGUAGE OverloadedStrings #-}
module Parser.GraphViz where

import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Data.Map as Map 
import qualified Data.Char as Char
import Data.Maybe (fromMaybe)
import Data.List (isSuffixOf, isPrefixOf,partition)
import Data.Functor (void)
import Debug.Trace (trace)



type VariablesGV = Map.Map String Int

data GraphViz = GraphViz {
    places :: Map.Map String [String],  -- non-highlevel places will be found in the simple entry of the map. 
                                        -- The highlevel places will be in the entry coresponding its variable: For Pr<N>, PR will be found in places[N]
    variables :: VariablesGV,             
    transitionsGV :: Map.Map String [(String,String)],
                                        -- Firsly all transitions are non-highlevel
                                        -- When parsing arcs, if a transition has an arc with a highlevel place, e.g. PR<N> then it will be moved into places[N]
                                        -- (String, String): T1 [time=(2,5)] -> ("T1","(2,5)"). For T2 -> ("T2","")
    arc :: [(String,String,String,String,Bool)]
                                        -- (String,String,String,String,Bool) --> (src,tgt,weight,inh,tgt)
                                        -- T1 -> P3 [weight=3] [inh=4] --> ("T1", "P3","3","inh=4",False)
                                        -- PF<N> -> T2 [weight={red:2,blue:3}] --> ("PF", "T2","{red:2,blue:3}","",True) 
                                      
} deriving (Show)


symbol :: String -> Parser String
symbol s = spaces *> string s <* spaces

lexeme :: Parser a -> Parser a
lexeme p = spaces *> p <* spaces

identifier :: Parser String
identifier = lexeme $ many1 (alphaNum <|> oneOf "_<>")

brackets :: Parser a -> Parser a
brackets = between (char '[') (char ']')

spacesOrComments :: Parser ()
spacesOrComments = void $ many (void (oneOf " \t\n") <|> void (char '/' *> manyTill anyChar newline))

line :: Parser a -> Parser a
line p = spaces *> p <* many (try spacesOrComments <|> void newline)


parseVariablesGV :: Parser VariablesGV
parseVariablesGV = do
    _ <- string "~VARIABLES" >> newline
    varLines <- manyTill (try parseVarDefGV) (lookAhead (try $ char '~'))
    return $ Map.fromList varLines

parseVarDefGV :: Parser (String, Int)
parseVarDefGV = do
    var <- identifier
    _ <- symbol "="
    val <- read <$> many1 digit
    _ <- many (noneOf "\n") >> newline
    return (var, val)

parsePlacesGV :: VariablesGV -> Parser (Map.Map String [String])
parsePlacesGV vars = do
    _ <- string "~PLACES" >> newline
    placeLines <- manyTill (try (parsePlaceGV vars)) (lookAhead (try $ char '~'))
    return $ foldr insertPlace Map.empty placeLines
  where
    insertPlace (name, isHL) acc
        | isHL = let (base, var) = span (/= '<') name
                     key = extractVar var
                 in Map.insertWith (++) key [base] acc
        | otherwise = Map.insertWith (++) "simple" [name] acc

    extractVar s = let inside = takeWhile (/= '>') (drop 1 s) in inside

parsePlaceGV :: VariablesGV -> Parser (String, Bool)
parsePlaceGV _ = do
    name <- identifier
    _ <- optional $ lexeme $ brackets (many (noneOf "]"))
    _ <- optional spacesOrComments
    return (name, "<" `isSuffixOf` name || '<' `elem` name)

parseTransitionsGV :: Parser (Map.Map String [(String, String)])
parseTransitionsGV = do
    _ <- string "~TRANSITIONS" >> newline
    transLines <- manyTill (try parseTransitionGV) (lookAhead (try $ char '~'))
    return $ Map.fromList [("simple",transLines)]

parseTransitionGV :: Parser (String, String)
parseTransitionGV = do
    name <- identifier
    time <- option "" (lexeme $ brackets $ string "time=" *> many (noneOf "]"))
    _ <- optional spacesOrComments
    return (name, time)

parseArcsGV :: Map.Map String String -> Parser [(String, String, String, String, Bool)]
parseArcsGV places= do
    _ <- string "~ARCS" >> newline
    manyTill (parseArcGV places) eof

parseArcGV :: Map.Map String String -> Parser (String, String, String, String, Bool)
parseArcGV places = do
    src <- identifier
    _ <- symbol "->"
    tgt <- identifier
    attrs <- many (lexeme $ brackets $ many (noneOf "]"))
    let (weight, inh) = extractAttrs attrs
        srcIsPlace = (stripHL tgt) `elem` (Map.keysSet places)
    _ <- optional spacesOrComments
    return (stripHL src, stripHL tgt, weight, inh, srcIsPlace)

extractAttrs :: [String] -> (String, String)
extractAttrs = foldr go ("1", "")
  where
    go s (w, i)
        | "weight=" `isPrefixOf` s = (drop 7 s, i)
        | "inh=" `isPrefixOf` s = (w, s)
        | otherwise = (w, i)

stripHL :: String -> String
stripHL name = case span (/= '<') name of
    (base, "") -> base
    (base, _)  -> base

skipNetType :: Parser ()
skipNetType = do
    _ <- string "~NET_TYPE" >> newline
    _ <- manyTill anyChar (try (newline >> lookAhead (char '~')))
    return ()


parseGraphViz :: Parser GraphViz
parseGraphViz = do
    skipNetType
    vars <- parseVariablesGV
    pls <- parsePlacesGV vars
    let places = reverseMapIgnoreKey "" pls
    trans <- parseTransitionsGV 
    arcs <- parseArcsGV places
    let transHL = foldr (addHLTransition pls) trans arcs
    return $ GraphViz pls vars transHL arcs

addHLTransition :: Map.Map String [String] 
                -> (String, String, String, String, Bool) 
                -> Map.Map String [(String, String)] 
                -> Map.Map String [(String, String)]
addHLTransition placeMap (src, tgt, _, _, _) transMap =
    let involved = [src, tgt]
        hlPlaceVars = [k | (k, vs) <- Map.toList placeMap, k /= "simple", any (`elem` vs) involved]

        -- Extract the (tgt, val) pair from "simple" if exists
        simpleList = Map.findWithDefault [] "simple" transMap
        (removedPairs, remainingSimple) = partition (\(x, _) -> x == tgt) simpleList
        -- removedPairs will have all pairs where first element == tgt (likely one)

        cleanedTransMap = Map.insert "simple" remainingSimple transMap

        -- The pairs to insert into each k: removedPairs or empty if none found
        pairsToAdd = removedPairs
    in case hlPlaceVars of
        [] -> transMap
        ks -> foldr
                (\k -> Map.insertWith (++) k pairsToAdd)
                cleanedTransMap
                ks


reverseMapIgnoreKey :: String -> Map.Map String [String] -> Map.Map String String
reverseMapIgnoreKey ignoreKey originalMap =
  let
    filtered = Map.filterWithKey (\k _ -> k /= ignoreKey) originalMap
    reversedPairs = concatMap (\(k, vs) -> map (\v -> (v, k)) vs) (Map.toList filtered)
  in Map.fromList reversedPairs