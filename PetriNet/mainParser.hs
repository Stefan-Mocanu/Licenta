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
