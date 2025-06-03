{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

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
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Debug.Trace
import Parser.GraphViz
import Parser.DotGen

data NetType = NetType {
    isColored   :: Bool,
    isTimed     :: Bool,
    isInhibited :: Bool,
    isHighLevel :: Bool,
    isColoredOriginal :: Bool
} deriving (Show, Generic)

data Variables = Variables{
    vars :: Map.Map String Int
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
    colors  :: [String],
    variables:: Variables
} deriving (Show, Generic)
-- instance ToJSON PetriNet

instance ToJSON Variables where
    toJSON (Variables vars) = toJSON vars

instance ToJSON NetType where
    toJSON (NetType colored timed inhibited highlevel _) = object
        [ "isColored"   .= colored
        , "isTimed"     .= timed
        , "isInhibited" .= inhibited
        , "isHighLevel" .= highlevel
        ]
instance ToJSON Place where
    toJSON (Place placeholder) = object ["placeholder" .= placeholder]

instance ToJSON Arc where
  toJSON (Arc place value inhibitor) =
    object $ ["place" .= place, "value" .= value]
            ++ maybe [] (\inh -> ["inhibitor" .= inh]) inhibitor

instance ToJSON Transition where
    toJSON (Transition input output time) = object $
        [ "input"  .= input
        , "output" .= output
        ] ++ case time of
                Just (minT, maxT) -> 
                    [ "minTime" .= minT
                    , "maxTime" .= maxT
                    ]
                Nothing -> []

instance ToJSON Net where
    toJSON (Net places transitions) = object [
        "places" .= map fst places,
        "transitions" .= Map.fromList transitions]

instance ToJSON M0 where
    toJSON (M0 (Left simple)) = object ["content" .= simple]
    toJSON (M0 (Right colored)) = object ["content" .= colored]

instance ToJSON PetriNet where
    toJSON (PetriNet net m0 netType colors variables) = 
        let colorField = if isColored netType then Just ("colors" .= colors) else Nothing
        in object (["net" .= net
                    , "m0" .= m0
                    , "netType" .= netType
                    , "variables" .= variables] ++ maybe [] (:[]) colorField)


instance ToJSON TimeInterval where 
    toJSON (TimeInterval minTime maxTime) = object [
        "minTime" .=  minTime,
        "maxTime" .= maxTime]


parsePlace :: Bool -> Bool -> Variables -> Parser [(String, Place, Int, Maybe (Map.Map String Int))]
parsePlace isColored isHighLevel (Variables varMap) = do
    name <- many1 alphaNum
    nameSuffix <- optionMaybe (between (char '<') (char '>') (many1 alphaNum))
    spaces
    tokenCount <- optionMaybe (try (if isColored 
                                    then parseColoredTokens 
                                    else parseSimpleTokens))
    spacesOrComments

    case nameSuffix of
        Just varName ->
            case Map.lookup varName varMap of
                Just n ->
                    let expanded = [ case tokenCount of
                                        Just (Right cmap) ->  -- Colored
                                            let extendedTokens = Map.insert (varName ++ "_" ++ show i) 1 cmap
                                            in (name ++ "_" ++ show i, Place 0, 0, Just extendedTokens)
                                        Just (Left count) -> -- Simple
                                             (name ++ "_" ++ show i, Place 0, count, Just (Map.fromList [("token", count)]))
                                        Nothing ->  -- No tokens specified
                                            (name ++ "_" ++ show i, Place 0, 0, Nothing)
                                   | i <- [1 .. n] ]
                    in return expanded
                Nothing -> fail $ "Variable " ++ varName ++ " not found in Variables map"
        Nothing -> 
            case tokenCount of
                Just (Left count) -> return [(name, Place 0, count, Just (Map.fromList [("token", count)]))]
                Just (Right cmap) -> return [(name, Place 0, 0, Just cmap)]
                Nothing           -> return [(name, Place 0, 0, Nothing)]



-- [tokens=2]
parseSimpleTokens :: Parser (Either Int (Map.Map String Int))
parseSimpleTokens = do
    _ <- lexeme (char '[')
    _ <- lexeme (string "tokens=")
    n <- many1 digit
    _ <- lexeme (char ']')
    return $ Left (read n)

-- [tokens={red=4,blue=10}]
parseColoredTokens :: Parser (Either Int (Map.Map String Int))
parseColoredTokens = do
    _ <- lexeme (char '[')
    _ <- lexeme (string "tokens=")
    _ <- lexeme (char '{')
    pairs <- sepBy parseColorPair (char ',')
    _ <- lexeme (char '}')
    _ <- lexeme (char ']')
    return $ Right (Map.fromList pairs)

parseColorPair :: Parser (String, Int)
parseColorPair = do
    color <- many1 alphaNum
    _ <- char ':'
    n <- many1 digit
    return (color, read n)

parseVariable :: Parser (String, Int)
parseVariable = do
    color <- many1 alphaNum
    spaces
    _ <- char '='
    spaces
    n <- many1 digit
    spacesOrComments
    return (color, read n)

parseVariables :: Parser Variables
parseVariables = do
    spacesOrComments
    string "~VARIABLES"
    spacesOrComments
    pairs <-manyTill parseVariable (lookAhead (string "~PLACES"))
    return $ Variables{
        vars = Map.fromList pairs
    }

parseNetType :: Parser NetType
parseNetType = do
    _ <- string "~NET_TYPE"
    _ <- newline
    flags <- manyTill anyChar newline
    let types = words flags
    return $ NetType
        { isColored = "COLORED" `elem` types || "HIGH_LEVEL" `elem` types
        , isTimed = "TIMED" `elem` types
        , isInhibited = "INHIBITED" `elem` types
        , isHighLevel = "HIGH_LEVEL" `elem` types
        , isColoredOriginal = "COLORED" `elem` types
        }

parseTime :: Parser TimeInterval
parseTime = do
    _ <- char '[' >> spaces >> string "time=" >> char '('
    minT <- many1 digit
    _ <- char ',' >> spaces
    maxT <- many1 digit
    _ <- char ')' >> char ']'
    return $ TimeInterval (read minT) (read maxT)

parseTransition :: Bool -> Bool -> Variables -> Parser [(String, Maybe (Int, Int))]
parseTransition isTimed isHighLevel (Variables varMap) = do
    name <- many1 alphaNum
    spaces
    time <- if isTimed then optionMaybe parseTime else return Nothing
    newline
    let interval = fmap (\(TimeInterval minT maxT) -> (minT, maxT)) time
    let result = if isHighLevel
                 then [ (name ++"_"++ key ++"_"++ show i, interval)
                      | (key, value) <- Map.toList varMap
                      , i <- [1 .. value] ]
                 else [ (name, interval) ]
    return result


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

extendWeight :: Weight -> String -> Weight
extendWeight (Colored m) key = Colored (Map.insert key 1 m)
extendWeight (Simple n) key      = Colored (Map.fromList [(key, 1),("token",n)])  -- or error if you prefer stricter behavior

changeInhibitor :: Maybe Inhibitor -> Maybe Inhibitor
changeInhibitor (Just(InhSimple inh)) = Just (InhColored (Map.fromList [("token", inh)]))
changeInhibitor Nothing = Nothing

parseArc :: Bool -> Bool -> Bool -> Variables -> [String] -> [String] -> Parser [(String, Arc, Bool)]
parseArc isColored isInhibited isHighLevel (Variables varMap) placeNames transitionNames = do
    src <- many1 alphaNum
    srcVar <- optionMaybe (between (char '<') (char '>') (many1 alphaNum))
    spaces
    _ <- string "->"
    spaces
    tgt <- many1 alphaNum
    tgtVar <- optionMaybe (between (char '<') (char '>') (many1 alphaNum))
    spaces

    weight <- if isHighLevel || isColored
                then do
                    m <- if isColored
                           then parseColorMap  -- m :: Map String Int
                           else do
                               w <- parseIntWeight
                               return w  -- Also Map String Int
                    return m  -- Now m is the right type
                else do
                    w <- parseIntWeight
                    return w
    spaces
    inh_aux <- if isInhibited
        then optionMaybe (try (lookAhead (char '[' >> string "inh=") >> spaces >> parseInhibitor isColored))
        else return Nothing
    spacesOrComments

    let inh = if (not isColored) && isInhibited && isHighLevel 
                then changeInhibitor inh_aux
                else inh_aux


    let isHighLevelSrc = isJust srcVar
        isHighLevelTgt = isJust tgtVar
        isSrcPlace = isHighLevelSrc || src `elem` placeNames
        isTgtPlace = isHighLevelTgt || tgt `elem` placeNames

    let arcDirection True False = Just (tgt, \s t w -> (t, Arc s w inh, True))
        arcDirection False True = Just (src, \s t w -> (s, Arc t w inh, False))
        arcDirection _     _     = Nothing

    case arcDirection isSrcPlace isTgtPlace of
      Just (fixedEnd, buildArc) -> do
        let expandVar v =
              case Map.lookup v varMap of
                Just n -> [1..n]
                Nothing -> []

        case (isHighLevel, srcVar, tgtVar) of
          (True, Just pv, _) | isSrcPlace -> do
              let indices = expandVar pv
              return [ buildArc (src ++ "_" ++ show i)
                                 (tgt ++ "_" ++ pv ++ "_" ++ show i)
                                 (extendWeight weight (pv ++ "_" ++ show i)) | i <- indices ]

          (True, _, Just pv) | isTgtPlace -> do
              let indices = expandVar pv
              return [ buildArc (src ++ "_" ++ pv ++ "_" ++ show i)
                                 (tgt ++ "_" ++ show i)
                                 (extendWeight weight (pv ++ "_" ++ show i)) | i <- indices ]

          (True, Nothing, Nothing) -> do
              let expanded = [ (v, i) | (v, maxV) <- Map.toList varMap, i <- [1..maxV] ]
              if isSrcPlace then
                  return [ buildArc src
                                     (tgt ++ "_" ++ v ++ "_" ++ show i)
                                     (extendWeight weight (v ++ "_" ++ show i))
                         | (v, i) <- expanded ]
              else if isTgtPlace then
                  return [ buildArc (src ++ "_" ++ v ++ "_" ++ show i)
                                     tgt
                                     (extendWeight weight (v ++ "_" ++ show i))
                         | (v, i) <- expanded ]
              else
                  fail $ "Could not determine arc direction for " ++ src ++ " -> " ++ tgt

          _ -> return [buildArc src tgt weight]

      Nothing -> fail $ "Could not determine arc direction for " ++ src ++ " -> " ++ tgt


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

hasBothIO :: (String, [Arc], [Arc]) -> Bool
hasBothIO (_, ins, outs) = not (null ins) && not (null outs)

parseNet :: Parser PetriNet
parseNet = do
    netType <- option (NetType False False False False False) (try parseNetType)
    variables <- option (Variables (Map.fromList [])) (try parseVariables)
    
    string "~PLACES"
    spacesOrComments
    placesGroups <- many (parsePlace (isColoredOriginal netType) (isHighLevel netType) variables)
    let placesListWithTokens = concat placesGroups

    let placesList = [(name, place) | (name, place, _, _) <- placesListWithTokens]
        marking = if isColored netType
            then Right (Map.fromList [(name, cmap) | (name, _, _, Just cmap) <- placesListWithTokens])
            else Left (Map.fromList [(name, token) | (name, _, token, _) <- placesListWithTokens])

    string "~TRANSITIONS"
    spacesOrComments
    transitionGroups <- many (parseTransition (isTimed netType) (isHighLevel netType) variables)
    let transitionsList = concat transitionGroups
    spacesOrComments
    string "~ARCS"
    spacesOrComments

    let placeNames = map (\(n, _) -> n) placesList
        transitionNames = map fst transitionsList
    
    arcs <- concat <$> many (parseArc (isColoredOriginal netType) (isInhibited netType) (isHighLevel netType) variables placeNames transitionNames)

    -- Organize parsed data
    let groupedTransitions = foldr groupArcs [] arcs
    let filteredTransitions =  filter hasBothIO groupedTransitions
    let timeMap = Map.fromList [(t, time) | (t, Just time) <- transitionsList]
    let transitionsMap = [(t, Transition ins outs (Map.lookup t timeMap))
                     | (t, ins, outs) <- filteredTransitions]
    let colorMap = case marking of
                    Right cmap -> cmap
                    Left _     -> Map.empty
        colors = Set.toList (collectColors (Net placesList transitionsMap) colorMap)
    return $ PetriNet (Net placesList transitionsMap) (M0 marking) netType colors variables

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
checkCondition cond (NetType colored timed inhibited _ _) =
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
        Right (PetriNet _ _ netType _ _) -> do
            let templateFile = "template.txt"
            exists <- doesFileExist templateFile
            if not exists
                then putStrLn $ "Template file not found: " ++ templateFile
                else do
                    template <- TIO.readFile templateFile
                    let output = processTemplate template netType
                    TIO.writeFile "output.go" output
                    putStrLn "Go file generated successfully as output.go"

generateGraphviz :: FilePath -> IO ()
generateGraphviz fileName = do
    content <- readFile fileName
    case parse parseGraphViz fileName content of
        Left err -> print err
        Right gv -> TIO.writeFile "output.dot" (T.pack (graphVizToDot gv))

    
main :: IO ()
main = do
    args <- getArgs
    case args of
        ["compile", fileName] -> compileCommand fileName
        ["generate-go", fileName] -> generateGoCommand fileName
        ["graphviz", fileName] -> generateGraphviz fileName
        _ -> putStrLn "Usage: petricli (compile|generate-go) filename" >> exitFailure