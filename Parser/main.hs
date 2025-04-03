{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, encode, toJSON, object, (.=))
import qualified Data.ByteString.Lazy.Char8 as B
import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Monad (void)
import Data.Char (isAlpha)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

-- Define Place, Transition, and Arc structures
data Place = Place { placeholder :: Int } deriving (Show, Generic)
-- instance ToJSON Place

data Arc = Arc { place :: String, value :: Int } deriving (Show, Generic)
-- instance ToJSON Arc

data Transition = Transition {
    input  :: [Arc],
    output :: [Arc]
} deriving (Show, Generic)
-- instance ToJSON Transition

data Net = Net {
    places      :: [(String, Place)],
    transitions :: [(String, Transition)]
} deriving (Show, Generic)
-- instance ToJSON Net

data M0 = M0 {
    content :: Map.Map String Int
} deriving (Show, Generic)
-- instance ToJSON M0

data PetriNet = PetriNet {
    net :: Net,
    m0  :: M0
} deriving (Show, Generic)
-- instance ToJSON PetriNet

instance ToJSON Place where
    toJSON (Place placeholder) = object ["placeholder" .= placeholder]

instance ToJSON Arc where
    toJSON (Arc place value) = object ["place" .= place, "value" .= value]

instance ToJSON Transition where
    toJSON (Transition input output) = object [
        "input" .= input,
        "output" .= output]

instance ToJSON Net where
    toJSON (Net places transitions) = object [
        "places" .= Map.fromList places,         -- Convert list to object
        "transitions" .= Map.fromList transitions] -- Convert list to object

instance ToJSON M0 where
    toJSON (M0 content) = object ["content" .= content]

instance ToJSON PetriNet where
    toJSON (PetriNet net m0) = object [
        "net" .= net,
        "m0" .= m0]


spacesOrComments :: Parser ()
spacesOrComments = void $ many (void (oneOf " \t\n") <|> void (char '/' *> manyTill anyChar newline))


parsePlace = do
    name <- many1 alphaNum
    spaces
    tokenCount <- optionMaybe (char '[' *> string "tokens=" *> many1 digit <* char ']')
    spacesOrComments
    let placeholderValue = 0
        tokenValue = fromMaybe 0 (tokenCount >>= readMaybe)  -- Default to 0 if missing
    return (name, Place placeholderValue, tokenValue)


parseTransition :: Parser String
parseTransition = do
    name <- many1 alphaNum
    spacesOrComments
    return name

parseArc :: [(String, Place)] -> Parser (String, Arc, Bool)
parseArc placesList = do  -- Now takes placesList as input
    src <- many1 alphaNum
    spaces
    string "->"
    spaces
    dest <- many1 alphaNum
    spaces
    weight <- option "1" (char '[' *> string "weight=" *> many1 digit <* char ']')
    spacesOrComments
    let isInput = src `elem` [name | (name, _) <- placesList] 
    return $ if isInput
        then (dest, Arc src (read weight), isInput)
        else (src, Arc dest (read weight), isInput)

parseNet :: Parser PetriNet
parseNet = do
    string "~PLACES"
    spacesOrComments
    placesListWithTokens <- many parsePlace

    let placesList = [(name, place) | (name, place, _) <- placesListWithTokens]
        marking = Map.fromList [(name, token) | (name, _, token) <- placesListWithTokens]  -- Fill marking dynamically

    string "~TRANSITIONS"
    spacesOrComments
    transitionsList <- many parseTransition

    string "~ARCS"
    spacesOrComments
    arcs <- many (parseArc placesList)

    -- Organize parsed data
    let groupedTransitions = foldr groupArcs [] arcs
        transitionsMap = [(t, Transition ins outs) | (t, ins, outs) <- groupedTransitions]

    return $ PetriNet (Net placesList transitionsMap) (M0 marking)

groupArcs :: (String, Arc, Bool) -> [(String, [Arc], [Arc])] -> [(String, [Arc], [Arc])]
groupArcs (t, arc, isInput) [] = [(t, [arc | isInput], [arc | not isInput])]
groupArcs (t, arc, isInput) ((t', ins, outs):rest)
    | t == t'   = (t, if isInput then arc:ins else ins, if isInput then outs else arc:outs) : rest
    | otherwise = (t', ins, outs) : groupArcs (t, arc, isInput) rest


main :: IO ()
main = do
    input <- readFile "sintax.stefan"
    case parse parseNet "" input of
        Left err -> print err
        Right petriNet -> B.putStrLn (encode petriNet)
