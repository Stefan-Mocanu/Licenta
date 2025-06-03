{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module PetriNet.Types where
    import Data.Map (Map)
    import Data.Aeson (ToJSON, encode, toJSON, object, (.=))

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