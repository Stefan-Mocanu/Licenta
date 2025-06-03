module PetriNet.Parser where
    import Text.ParserCombinators.Parsec
    import PetriNet.Types

spacesOrComments :: Parser ()
spacesOrComments = void $ many (void (oneOf " \t\n") <|> void (char '/' *> manyTill anyChar newline))


parsePlace :: Bool -> Bool -> Variables -> Parser [(String, Place, Int, Maybe (Map.Map String Int))]
parsePlace isColored isHighLevel (Variables varMap) = do
    name <- many1 alphaNum
    nameSuffix <- optionMaybe (between (char '<') (char '>') (many1 alphaNum))
    spaces
    tokenCount <- optionMaybe (try (if isColored then parseColoredTokens else parseSimpleTokens))
    spacesOrComments

    case nameSuffix of
        Just varName ->
            case Map.lookup varName varMap of
                Just n ->
                    let expanded = [ 
                            if isColored 
                                then
                                    let baseTokens = case tokenCount of
                                                        Just (Right cmap) -> cmap
                                                        _ -> Map.empty
                                        extendedTokens = Map.insert (varName ++ "_" ++ show i) 1 baseTokens
                                    in (name ++ "_" ++ show i, Place 0, 0, Just extendedTokens)
                                else
                                    let baseCount = case tokenCount of
                                                        Just (Left c) -> c
                                                        _ -> 0
                                    in (name ++ "_" ++ show i, Place 0, baseCount, Just (Map.fromList [((varName ++ "_" ++ show i), 1),("token", baseCount)]))
                          | i <- [1 .. n] ]
                    in return expanded
                Nothing -> fail $ "Variable " ++ varName ++ " not found in Variables map"
        Nothing ->
            let tokenValue = case tokenCount of
                    Just (Left count) -> count
                    _ -> 0
                colorTokens = case tokenCount of
                    Just (Right cmap) -> Just cmap
                    _ -> Nothing
            in return [(name, Place 0, tokenValue, Just (Map.fromList [("token",tokenValue)]))]


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
