{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, encode, toJSON, object, (.=))
import qualified Data.ByteString.Lazy.Char8 as B
import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)  
import System.Environment (getArgs)
import System.Exit (die, exitFailure)
import System.Directory (doesFileExist)
import Data.List (isPrefixOf, isSuffixOf, intercalate)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Debug.Trace

data NetType = NetType {
    isColored   :: Bool,
    isTimed     :: Bool,
    isInhibited :: Bool
} deriving (Show, Generic)

-- Define Place, Transition, and Arc structures
data Place = Place { placeholder :: Int } deriving (Show, Generic)
-- instance ToJSON Place

data Weight = Simple Int | Colored (Map.Map String Int) deriving (Show, Generic)

instance ToJSON Weight where
    toJSON (Simple i) = toJSON i
    toJSON (Colored m) = toJSON m

data Inhibitor = InhSimple Int | InhColored (Map.Map String Int) deriving (Show, Generic)

instance ToJSON Inhibitor where
  toJSON (InhSimple i) = toJSON i
  toJSON (InhColored m) = toJSON m

data TimeInterval = TimeInterval
  { minTime :: Int
  , maxTime :: Int
  } deriving (Show, Generic)



data Arc = Arc 
    { place :: String
    , value :: Weight
    , inhibitor :: Maybe Inhibitor
    } deriving (Show, Generic)
-- instance ToJSON Arc

data Transition = Transition {
    input  :: [Arc],
    output :: [Arc],
    time :: Maybe (Int, Int)
} deriving (Show, Generic)
-- instance ToJSON Transition

data Net = Net {
    places      :: [(String, Place)],
    transitions :: [(String, Transition)]
    
} deriving (Show, Generic)
-- instance ToJSON Net

data M0 = M0 {
    content :: Either (Map.Map String Int) (Map.Map String (Map.Map String Int))
} deriving (Show, Generic)
-- instance ToJSON M0

data PetriNet = PetriNet {
    net     :: Net,
    m0      :: M0,
    netType :: NetType,
    colors  :: [String]
} deriving (Show, Generic)
-- instance ToJSON PetriNet


instance ToJSON NetType where
    toJSON (NetType colored timed inhibited) = object
        [ "isColored"   .= colored
        , "isTimed"     .= timed
        , "isInhibited" .= inhibited
        ]
instance ToJSON Place where
    toJSON (Place placeholder) = object ["placeholder" .= placeholder]

instance ToJSON Arc where
  toJSON (Arc place value inhibitor) =
    object $ ["place" .= place, "value" .= value]
            ++ maybe [] (\inh -> ["inhibitor" .= inh]) inhibitor

instance ToJSON Transition where
    toJSON (Transition input output time) = object $ [
        "input" .= input,
        "output" .= output] 
        ++ maybe [] (\(minT, maxT) -> ["time" .= object ["minTime" .= minT, "maxTime" .= maxT]]) time

instance ToJSON Net where
    toJSON (Net places transitions) = object [
        "places" .= map fst places,
        "transitions" .= Map.fromList transitions]

instance ToJSON M0 where
    toJSON (M0 (Left simple)) = object ["content" .= simple]
    toJSON (M0 (Right colored)) = object ["content" .= colored]

instance ToJSON PetriNet where
    toJSON (PetriNet net m0 netType colors) = 
        let colorField = if isColored netType then Just ("colors" .= colors) else Nothing
        in object (["net" .= net, "m0" .= m0, "netType" .= netType] ++ maybe [] (:[]) colorField)


instance ToJSON TimeInterval where 
    toJSON (TimeInterval minTime maxTime) = object [
        "minTime" .=  minTime,
        "maxTime" .= maxTime]

spacesOrComments :: Parser ()
spacesOrComments = void $ many (void (oneOf " \t\n") <|> void (char '/' *> manyTill anyChar newline))


parsePlace :: Bool -> Parser (String, Place, Int, Maybe (Map.Map String Int))
parsePlace isColored = do
    name <- many1 alphaNum
    spaces
    tokenCount <- optionMaybe (try (if isColored then parseColoredTokens else parseSimpleTokens))
    spacesOrComments
    let placeholderValue = 0
        tokenValue = case tokenCount of
            Just (Left count) -> count
            _ -> 0
        colorTokens = case tokenCount of
            Just (Right cmap) -> Just cmap
            _ -> Nothing
    return (name, Place placeholderValue, tokenValue, colorTokens)

-- [tokens=2]
parseSimpleTokens :: Parser (Either Int (Map.Map String Int))
parseSimpleTokens = do
    _ <- char '[' >> string "tokens="
    n <- many1 digit
    _ <- char ']'
    return $ Left (read n)

-- [tokens={red=4,blue=10}]
parseColoredTokens :: Parser (Either Int (Map.Map String Int))
parseColoredTokens = do
    _ <- char '[' >> string "tokens={"
    pairs <- sepBy parseColorPair (char ',')
    _ <- string "}]"
    return $ Right (Map.fromList pairs)

parseColorPair :: Parser (String, Int)
parseColorPair = do
    color <- many1 alphaNum
    _ <- char '='
    n <- many1 digit
    return (color, read n)


parseNetType :: Parser NetType
parseNetType = do
    _ <- string "~NET_TYPE"
    _ <- newline
    flags <- manyTill anyChar newline
    let types = words flags
    return $ NetType
        { isColored = "COLORED" `elem` types
        , isTimed = "TIMED" `elem` types
        , isInhibited = "INHIBITED" `elem` types
        }

parseTime :: Parser TimeInterval
parseTime = do
    _ <- char '[' >> spaces >> string "time=" >> char '('
    minT <- many1 digit
    _ <- char ',' >> spaces
    maxT <- many1 digit
    _ <- char ')' >> char ']'
    return $ TimeInterval (read minT) (read maxT)

parseTransition :: Bool -> Parser (String, Maybe (Int, Int))
parseTransition isTimed = do
    name <- many1 alphaNum
    spaces
    time <- if isTimed then optionMaybe parseTime else return Nothing
    newline
    return (name, fmap (\(TimeInterval minT maxT) -> (minT, maxT)) time)

-- Parser for a simple weight like [weight=2]
parseIntWeight :: Parser Weight
parseIntWeight = do
    _ <- char '[' >> string "weight="
    n <- many1 digit
    _ <- char ']'
    return $ Simple (read n)

-- Parser for a color map like [weight={red=2,blue=3}]
parseColorMap :: Parser Weight
parseColorMap = do
    _ <- char '[' >> string "weight={"
    pairs <- sepBy parseColorPair (char ',')
    _ <- string "}]"
    return $ Colored (Map.fromList pairs)


parseInhibitor :: Bool -> Parser Inhibitor
parseInhibitor isColored = if isColored then parseColoredInhibitor else parseSimpleInhibitor

parseSimpleInhibitor :: Parser Inhibitor
parseSimpleInhibitor = do
  _ <- char '[' >> string "inh="
  n <- many1 digit
  _ <- char ']'
  return $ InhSimple (read n)

parseColoredInhibitor :: Parser Inhibitor
parseColoredInhibitor = do
  _ <- char '[' >> string "inh={"
  pairs <- sepBy parseColorPair (char ',')
  _ <- string "}]"
  return $ InhColored (Map.fromList pairs)

parseArc :: Bool -> Bool -> [String] -> [String] -> Parser (String, Arc, Bool)
parseArc isColored isInhibited placeNames transitionNames = do
    src <- many1 alphaNum
    spaces
    _ <- string "->"
    spaces
    tgt <- many1 alphaNum
    spaces
    weight <- if isColored then parseColorMap else parseIntWeight
    spaces
    inh <- if isInhibited
        then optionMaybe (try (lookAhead (char '[' >> string "inh=") >> spaces >> parseInhibitor isColored))
        else return Nothing
    spacesOrComments
    let isSrcPlace = src `elem` placeNames
        isTgtPlace = tgt `elem` placeNames
    case (isSrcPlace, isTgtPlace) of
      (True, False) -> -- input arc
        return (tgt, Arc src weight inh, True)
      (False, True) -> -- output arc
        return (src, Arc tgt weight inh, False)
      _ -> fail $ "Could not determine arc direction for " ++ src ++ " -> " ++ tgt

extractColorKeys :: [Map.Map String Int] -> Set.Set String
extractColorKeys = Set.unions . map Map.keysSet

collectColors :: Net -> Map.Map String (Map.Map String Int) -> Set.Set String
collectColors net marking =
    let initColors = extractColorKeys (Map.elems marking)

        arcColors = extractColorKeys
            [m | (_, t) <- transitions net
            , arc <- input t ++ output t
            , Colored m <- [value arc]]

        inhColors = extractColorKeys
            [m | (_, t) <- transitions net
            , arc <- input t ++ output t
            , Just (InhColored m) <- [inhibitor arc]]

    in Set.unions [initColors, arcColors, inhColors]

parseNet :: Parser PetriNet
parseNet = do
    netType <- option (NetType False False False) (try parseNetType)
    string "~PLACES"
    spacesOrComments
    placesListWithTokens <- many (parsePlace (isColored netType))

    let placesList = [(name, place) | (name, place, _, _) <- placesListWithTokens]
        marking = if isColored netType
            then Right (Map.fromList [(name, cmap) | (name, _, _, Just cmap) <- placesListWithTokens])
            else Left (Map.fromList [(name, token) | (name, _, token, _) <- placesListWithTokens])

    string "~TRANSITIONS"
    spacesOrComments
    transitionsList <- many (parseTransition (isTimed netType))
    spacesOrComments
    string "~ARCS"
    spacesOrComments

    let placeNames = map (\(n, _) -> n) placesList
        transitionNames = map fst transitionsList
    arcs <- many (parseArc (isColored netType) (isInhibited netType) placeNames transitionNames)

    -- Organize parsed data
    let groupedTransitions = foldr groupArcs [] arcs
    let timeMap = Map.fromList [(t, time) | (t, Just time) <- transitionsList]
    let transitionsMap = [(t, Transition ins outs (Map.lookup t timeMap))
                     | (t, ins, outs) <- groupedTransitions]
    let colorMap = case marking of
                    Right cmap -> cmap
                    Left _     -> Map.empty
        colors = Set.toList (collectColors (Net placesList transitionsMap) colorMap)
    return $ PetriNet (Net placesList transitionsMap) (M0 marking) netType colors

groupArcs :: (String, Arc, Bool) -> [(String, [Arc], [Arc])] -> [(String, [Arc], [Arc])]
groupArcs (t, arc, isInput) [] = [(t, [arc | isInput], [arc | not isInput])]
groupArcs (t, arc, isInput) ((t', ins, outs):rest)
    | t == t'   = (t, if isInput then arc:ins else ins, if isInput then outs else arc:outs) : rest
    | otherwise = (t', ins, outs) : groupArcs (t, arc, isInput) rest

processTemplate :: T.Text -> NetType -> T.Text
processTemplate text netType = solve text
  where
    solve :: T.Text -> T.Text
    solve t
      | T.null t = ""
      | T.isInfixOf "<<" t =
          let (beforeBlock, afterOpen) = T.breakOn "<<" t
              afterOpen' = T.drop 2 afterOpen  -- drop "<<"
              (condPart, afterComma) = T.breakOn "," afterOpen'
              afterComma' = T.drop 1 afterComma  -- drop ","
              (blockContent, afterClose) = T.breakOn ">>" afterComma'
              afterClose' = T.drop 2 afterClose  -- drop ">>"
              condition = T.strip condPart
          in if checkCondition condition netType
             then beforeBlock <> blockContent <> solve afterClose'
             else beforeBlock <> solve afterClose'
      | otherwise = t

checkCondition :: T.Text -> NetType -> Bool
checkCondition cond (NetType colored timed inhibited) =
    let c = T.toUpper cond
    in case c of
         "COLORED"               -> colored
         "NOT COLORED"           -> not colored
         "TIMED"                 -> timed
         "NOT TIMED"             -> not timed
         "INHIBITED"             -> inhibited
         "NOT INHIBITED"         -> not inhibited
         "TIMED AND COLORED"     -> timed && colored
         "NOT COLORED AND INHIBITED" -> not colored && inhibited
         "COLORED AND INHIBITED"   -> colored && inhibited
         "COLORED AND NOT INHIBITED" -> colored && not inhibited
         "NOT COLORED AND NOT INHIBITED" -> not colored && not inhibited
         "TIMED AND NOT COLORED" -> timed && not colored
         _                       -> False

compileCommand :: FilePath -> IO ()
compileCommand fileName = do
    content <- readFile fileName
    case parse parseNet fileName content of
        Left err -> do
            putStrLn "Failed to parse file:"
            print err
        Right petriNet -> do
            putStrLn "Parsed successfully:"
            B.putStrLn (encode petriNet)

generateGoCommand :: FilePath -> IO ()
generateGoCommand fileName = do
    content <- readFile fileName
    case parse parseNet fileName content of
        Left err -> do
            putStrLn "Failed to parse file:"
            print err
        Right (PetriNet _ _ netType _) -> do
            let templateFile = "template.txt"
            exists <- doesFileExist templateFile
            if not exists
                then putStrLn $ "Template file not found: " ++ templateFile
                else do
                    template <- TIO.readFile templateFile
                    let output = processTemplate template netType
                    TIO.writeFile "output.go" output
                    putStrLn "Go file generated successfully as output.go"

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["compile", fileName] -> compileCommand fileName
        ["generate-go", fileName] -> generateGoCommand fileName
        _ -> putStrLn "Usage: petricli (compile|generate-go) filename" >> exitFailure