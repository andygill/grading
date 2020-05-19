{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
import qualified Data.Csv as C
import qualified Data.Aeson as A
import           Data.Aeson (FromJSON, ToJSON, parseJSON, (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.List as L
import           Data.String as S
import qualified Data.Text as T
import qualified Data.Vector as V hiding ((++))
import qualified Data.Yaml as Y
import qualified Data.Yaml.Pretty as Y
import System.Environment

-- temp main

data AssignmentSchema = AssignmentSchema 
  { outOf  :: Double    -- /3 mean max score of 3
  , weight :: Double	-- 1 == 100% == everything
  }
  deriving Show

type AssignmentName = String

data Class = Class 
  { weights:: [(AssignmentName,AssignmentSchema)]
  , learning:: [(String,[AssignmentName])]
  , grading:: [(String,Double)]
  , students:: [Student]
  }    
  deriving Show


instance ToJSON Class where
  toJSON Class{..} = A.object $
    [ "Weights" .= A.object 
        [ T.pack n .= ("/" ++ show o ++ "," ++ show (w * 100) ++ "%") | (n,AssignmentSchema o w) <- weights ]
--    , "Learning Objectives" .= A.object 
--        [ show w | w <- learning ]
--    , "Grading" .= A.object 
--        [ show w | w <- grading ]
    ] ++ map studentToAttribute students


data Student = Student
  { name :: String -- Flintstone, Fred
  , kuid :: Integer
  , email :: String
  , degree :: Degree
  , assignments :: [(AssignmentName,Score)]
  }
  deriving Show

instance ToJSON Student where
  toJSON st = A.object [studentToAttribute st]

studentToAttribute (Student{..}) = T.pack name .= A.object 
    ([ "kuid" .= kuid 
     , "email" .= email
     , "degree" .= show degree
     ] ++
     [ T.pack name .= n
     | (name,Score n) <- assignments
     ])

--instance FromJSON Student where
--   parseJSON = withObject "Student" $ \ o -> 

newtype Students = Students [Student]

instance ToJSON Students where
  toJSON (Students ss) = A.object (map studentToAttribute ss)

data Degree = CSBS | CoEBS | EEBS | ICBS | CSBS_CEBS | MathBS | PhysBS | GRAD | CSMS | CoEMS | CSPhD
  deriving (Show, Read)

data Score = Score Double
  deriving Show

instance FromJSON Degree where
--  parseJSON (A.String "BSCS") = pure BSCS
  parseJSON other = fail $ show other

instance FromJSON Score where
  parseJSON v@A.Number{} = Score <$> parseJSON v
  parseJSON v = Score . sum . (++ []) <$> parseJSON v 
  parseJSON other = fail $ show other

main :: IO ()
main = do
  args <- getArgs 
  main2 args

main2 :: [String] -> IO ()
main2 ["import",csvFile] = do
  txt <- BSL.readFile csvFile
  let Right (csv :: V.Vector [String]) = C.decode C.NoHeader txt  
  print csv
  let students :: [Student] = [ s | Just s <- importStudent <$> V.toList csv ]
  BS.putStrLn $ encodeStudent (Students students)
main2 ["verify",yamlFile] = do
  r <- Y.decodeFileEither yamlFile
  case r of
   Right a -> print (a :: A.Value)
   Left msg -> print msg

importStudent :: [String] -> Maybe Student
importStudent [kuid,name,email,_,_,_,major_degree,_,"Enrolled"] = pure $ Student
  { name = name
  , kuid = read kuid
  , email = email
  , degree = readDegree major_degree
  , assignments = []
  }
  where
   readDegree "Engineering Undergraduate - Computer ScienceBS" = CSBS
   readDegree "Engineering Undergraduate - Computer ScienceBS/MathematicsMINOR" = CSBS
   readDegree "Engineering Undergraduate - Computer ScienceBS/BusinessMINOR/PsychologyMINOR" = CSBS
   readDegree "Engineering Undergraduate - Computer ScienceBS/Pre-Dr. Medicine/Osteopathic" = CSBS
   readDegree "Engineering Undergraduate - Computer ScienceBS/Pre-Law" = CSBS
   readDegree "Engineering Undergraduate - Computer ScienceBS/EconomicsMINOR" = CSBS
   readDegree "Engineering Undergraduate - Computer ScienceBS/BusinessMINOR" = CSBS
   readDegree "Engineering Undergraduate - Computer ScienceBS/PsychologyMINOR" = CSBS
   readDegree "Engineering Undergraduate - Computer ScienceBS/Computer EngineeringBS" = CSBS_CEBS
   readDegree "Liberal Arts&Sci Undergraduate - PhysicsBS" = PhysBS
   readDegree "Engineering Undergraduate - Interdisciplinary ComputingBS" = ICBS
   readDegree "Liberal Arts&Sci Undergraduate - MathematicsBS" = MathBS
   readDegree "Engineering Undergraduate - Computer EngineeringBS" = CoEBS
   readDegree "Engineering Undergraduate - Electrical EngineeringBS" = EEBS
   readDegree "Engineering Graduate - Computer ScienceMS/Computer SciencePHD" = GRAD
   readDegree "Engineering Graduate - Computer SciencePHD" = CSPhD
   readDegree "Engineering Graduate - Computer ScienceMS" = CSMS
   readDegree "Engineering Graduate - Computer EngineeringMS" = CoEMS
   readDegree d = error $ "degree: " ++ show d


importStudent _ = Nothing

encodeStudent :: ToJSON a => a -> BS.ByteString
encodeStudent = Y.encodePretty (Y.setConfCompare labels Y.defConfig)

labels :: (IsString a, Ord a) => a -> a -> Ordering
labels xs ys = case (L.elemIndex xs order, L.elemIndex ys order) of
    (Just x,Just y) -> x `compare` y
    (Just x,Nothing) -> LT
    (Nothing,Just x) -> GT
    _ -> xs `compare` ys
  where
    order = ["kuid","email","degree"]
