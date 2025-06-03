{-# LANGUAGE OverloadedStrings #-}

module PetriNet.JSON where
    import PetriNet.Types
    import Data.Aeson
    import Data.Aeson.Types
    import qualified Data.Text as T
    import Data.Map (toList)

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