module Joyride.Types where

import Prelude

import Data.Either (Either(..), hush)
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (class Newtype)
import Data.Reflectable (class Reflectable, reflectType)
import Foreign (ForeignError(..), fail)
import Record (union)
import Simple.JSON as JSON
import Type.Proxy (Proxy(..))

instance Show Column where
  show = JSON.writeJSON

derive instance Eq Column
derive instance Ord Column

data Position = Position1 | Position2 | Position3 | Position4

derive instance Eq Position
instance Show Position where
  show = JSON.writeJSON

instance JSON.ReadForeign Position where
  readImpl i = do
    ri <- JSON.readImpl i
    case ri of
      "Position1" -> pure Position1
      "Position2" -> pure Position2
      "Position3" -> pure Position3
      "Position4" -> pure Position4
      _ -> fail $ ForeignError ("No idea how to parse: " <> JSON.writeJSON i)

instance JSON.WriteForeign Position where
  writeImpl Position1 = JSON.writeImpl "Position1"
  writeImpl Position2 = JSON.writeImpl "Position2"
  writeImpl Position3 = JSON.writeImpl "Position3"
  writeImpl Position4 = JSON.writeImpl "Position4"

instance JSON.ReadForeign Column where
  readImpl x = do
    ri <- JSON.readImpl x
    case intToColumn ri of
      Right c -> pure c
      Left i -> fail $ ForeignError ("No idea how to parse column: " <> JSON.writeJSON i)

instance JSON.WriteForeign Column where
  writeImpl = JSON.writeImpl <<< columnToInt

data Column = C1 | C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9 | C10 | C11 | C12 | C13 | C14 | C15 | C16

instance Semigroup Column where
  append a b = fromMaybe C7 $ hush $ intToColumn ((((columnToInt a) + (columnToInt b) - 1) `mod` 16) + 1)

intToColumn :: Int -> Either Int Column
intToColumn 1 = Right C1
intToColumn 2 = Right C2
intToColumn 3 = Right C3
intToColumn 4 = Right C4
intToColumn 5 = Right C5
intToColumn 6 = Right C6
intToColumn 7 = Right C7
intToColumn 8 = Right C8
intToColumn 9 = Right C9
intToColumn 10 = Right C10
intToColumn 11 = Right C11
intToColumn 12 = Right C12
intToColumn 13 = Right C13
intToColumn 14 = Right C14
intToColumn 15 = Right C15
intToColumn 16 = Right C16
intToColumn x = Left x

columnToInt :: Column -> Int
columnToInt C1 = 1
columnToInt C2 = 2
columnToInt C3 = 3
columnToInt C4 = 4
columnToInt C5 = 5
columnToInt C6 = 6
columnToInt C7 = 7
columnToInt C8 = 8
columnToInt C9 = 9
columnToInt C10 = 10
columnToInt C11 = 11
columnToInt C12 = 12
columnToInt C13 = 13
columnToInt C14 = 14
columnToInt C15 = 15
columnToInt C16 = 16

-- | Beats, or a temporal unit based on seconds modulated by a tempo.
data Version (i :: Int) = Version Int

derive instance Eq (Version i)
derive instance Ord (Version i)

instance Reflectable i Int => Show (Version i) where
  show _ = "Version " <> show (reflectType (Proxy :: _ i))

instance Semigroup (Version i) where
  append a _ = a

instance Reflectable i Int => Monoid (Version i) where
  mempty = Version (reflectType (Proxy :: _ i))

instance Reflectable i Int => JSON.ReadForeign (Version i) where
  readImpl i' = do
    i <- JSON.readImpl i'
    let j = reflectType (Proxy :: _ i)
    if i == j then pure (Version i) else fail (ForeignError $ "Expecting version " <> show j <> " but received " <> show i)

instance Reflectable i Int => JSON.WriteForeign (Version i) where
  writeImpl _ = JSON.writeImpl (reflectType (Proxy :: _ i))

instance JSON.ReadForeign EventV0 where
  readImpl i = do
    { _type } :: { _type :: String } <- JSON.readImpl i
    case _type of
      "Basic" -> BasicEventV0 <$> JSON.readImpl i
      "Leap" -> LeapEventV0 <$> JSON.readImpl i
      "Long" -> LongEventV0 <$> JSON.readImpl i
      _ -> fail (ForeignError $ "Could not parse: " <> JSON.writeJSON i)

instance JSON.WriteForeign EventV0 where
  writeImpl (BasicEventV0 x) = JSON.writeImpl $ union { _type: "Basic" } x
  writeImpl (LeapEventV0 x) = JSON.writeImpl $ union { _type: "Leap" } x
  writeImpl (LongEventV0 x) = JSON.writeImpl $ union { _type: "Long" } x

data Event_ = EventV0 EventV0

type BasicEventV0' =
  { marker1Time :: Number
  , marker1AudioURL :: Maybe String
  , marker2Time :: Number
  , marker2AudioURL :: Maybe String
  , marker3Time :: Number
  , marker3AudioURL :: Maybe String
  , marker4Time :: Number
  , marker4AudioURL :: Maybe String
  , column :: Column
  , name :: Maybe String
  , version :: Version 0
  }

type LeapEventV0' =
  { marker1Time :: Number
  , marker2Time :: Number
  , audioURL :: Maybe String
  , column :: Column
  , position :: Position
  , name :: Maybe String
  , version :: Version 0
  }

type LongEventV0' =
  { marker1Time :: Number
  , marker2Time :: Number
  , audioURL :: Maybe String
  , length :: Number
  , column :: Column
  , name :: Maybe String
  , version :: Version 0
  }

data EventV0
  = BasicEventV0 BasicEventV0'
  | LeapEventV0 LeapEventV0'
  | LongEventV0 LongEventV0'

instance JSON.ReadForeign Event_ where
  readImpl i = EventV0 <$> (JSON.readImpl i)

instance JSON.WriteForeign Event_ where
  writeImpl (EventV0 i) = JSON.writeImpl i

newtype Whitelist = Whitelist (Array String)

-- we allow for whitelist to optionally be absent
instance JSON.ReadForeign Whitelist where
  readImpl i = Whitelist <$> (fromMaybe [] <$> JSON.readImpl i)

instance JSON.WriteForeign Whitelist where
  writeImpl (Whitelist i) = JSON.writeImpl i

derive instance Newtype Whitelist _
derive newtype instance Show Whitelist
derive newtype instance Eq Whitelist
derive newtype instance Ord Whitelist

type TrackV0' =
  { url :: String
  , private :: Boolean
  , title :: Maybe String
  , owner :: String
  , whitelist :: Whitelist
  , version :: Version 0
  }

data Track = TrackV0 TrackV0'

instance Show Track where
  show (TrackV0 t) = "TrackV0 <" <> show t <> ">"

instance JSON.ReadForeign Track where
  readImpl i = TrackV0 <$> (JSON.readImpl i)

instance JSON.WriteForeign Track where
  writeImpl (TrackV0 i) = JSON.writeImpl i
