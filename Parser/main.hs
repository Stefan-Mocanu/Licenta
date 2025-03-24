{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, encode)
import qualified Data.ByteString.Lazy.Char8 as B
import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Monad (void)
import Data.Char (toLower, isAlpha)

-- Define Place, Transition, and Arc structures
data Place = Place { placeholder :: Int } deriving (Show, Generic)
instance ToJSON Place

data Arc = Arc { place :: String, value :: Int } deriving (Show, Generic)
instance ToJSON Arc

data Transition = Transition {
    input  :: [Arc],
    output :: [Arc]
} deriving (Show, Generic)
instance ToJSON Transition

data Net = Net {
    places      :: [(String, Place)],
    transitions :: [(String, Transition)]
} deriving (Show, Generic)
instance ToJSON Net

data M0 = M0 {
    content :: [(String, Int)]
} deriving (Show, Generic)
instance ToJSON M0

data PetriNet = PetriNet {
    net :: Net,
    m0  :: M0
} deriving (Show, Generic)
instance ToJSON PetriNet

-- instance ToJSON Place where
--     toJSON (Place placeholder) = object ["placeholder" .= placeholder]

-- instance ToJSON Arc where
--     toJSON (Arc place value) = object ["place" .= place, "value" .= value]

-- instance ToJSON Transition where
--     toJSON (Transition input output) = object [
--         "input" .= input,
--         "output" .= output]

-- instance ToJSON Net where
--     toJSON (Net places transitions) = object [
--         "places" .= places,
--         "transitions" .= transitions]

-- instance ToJSON M0 where
--     toJSON (M0 content) = object ["content" .= content]

-- instance ToJSON PetriNet where
--     toJSON (PetriNet net m0) = object [
--         "net" .= net,
--         "m0" .= m0]


spacesOrComments :: Parser ()
spacesOrComments = void $ many (void (oneOf " \t\n") <|> void (char '/' *> manyTill anyChar newline))


parsePlace :: Parser (String, Place)
parsePlace = do
    name <- many1 alphaNum
    spaces
    tokenCount <- optionMaybe (char '[' *> string "tokens=" *> many1 digit <* char ']')
    spacesOrComments
    let placeholderValue = 0
    return (map toLower name, Place placeholderValue)


parseTransition :: Parser String
parseTransition = do
    name <- many1 alphaNum
    spacesOrComments
    return $ map toLower name

parseArc :: Parser (String, Arc, Bool)  -- Bool for input/output direction
parseArc = do
    src <- many1 alphaNum
    spaces
    dir <- string "->"
    spaces
    dest <- many1 alphaNum
    spaces
    weight <- option "1" (char '[' *> string "weight=" *> many1 digit <* char ']')
    spacesOrComments
    let isInput = all isAlpha src  -- If the source is a place, it's an input arc
    return (map toLower dest, Arc (map toLower src) (read weight), isInput)

parseNet :: Parser PetriNet
parseNet = do
    string "~PLACES"
    spacesOrComments
    placesList <- many parsePlace

    string "~TRANSITIONS"
    spacesOrComments
    transitionsList <- many parseTransition

    string "~ARCS"
    spacesOrComments
    arcs <- many parseArc

    -- Organize parsed data
    let groupedTransitions = foldr groupArcs [] arcs
        transitionsMap = [(t, Transition ins outs) | (t, ins, outs) <- groupedTransitions]

    -- Extract initial markings from places
    let marking = [(p, 4) | (p, _) <- placesList, p == "p1"] ++  -- Example for initial marking
                  [(p, 2) | (p, _) <- placesList, p == "p2"]

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
