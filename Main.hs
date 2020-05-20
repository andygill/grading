{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
import qualified Data.Csv as C
import qualified Data.Aeson as A
import           Data.Aeson (FromJSON, ToJSON, toJSON, parseJSON, (.=), (.:), withObject)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.List as L
import           Data.String as S
import           Data.Char
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Vector as V hiding ((++))
import qualified Data.Yaml as Y
import qualified Data.Yaml.Pretty as Y
import Control.Applicative
import           Text.Read
import System.Environment

-- represent key/value object as key/value pairs
newtype KeyValues a = KeyValues { unKeyValues :: [(String,a)] }
	deriving Show

instance ToJSON a => ToJSON (KeyValues a) where
  toJSON (KeyValues kvs) = A.object [ T.pack k .= v | (k,v) <- kvs ]

instance FromJSON a => FromJSON (KeyValues a) where
  parseJSON = withObject "KeyValues" $ \ o -> KeyValues <$>
  	    sequence [ parseJSON v >>= \ r -> pure (T.unpack k,r) | (k,v) <- H.toList o ]

data AssignmentSchema = AssignmentSchema 
  { score  :: Double            -- max score 
  , weight :: Percent 		-- weight of this assignment
  }
  deriving Show

instance ToJSON AssignmentSchema where
  toJSON (AssignmentSchema o w) = A.object 
     [ "score" .= o
     , "weight" .= w
     ]

instance FromJSON AssignmentSchema where
  parseJSON = withObject "AssignmentSchema" $ \ v -> AssignmentSchema
    <$> v .: "score"
    <*> v .: "weight"

type AssignmentName = String

data Class = Class 
  { weights  :: [(AssignmentName,AssignmentSchema)]
  , learning :: [(String,[AssignmentName])]
  , grading  :: [(String,Percent)]
  , students :: [Student]
  }    
  deriving Show

instance ToJSON Class where
  toJSON Class{..} = A.object $
    [ "Weights" .= A.object 
        [ T.pack n .= s
	| (n,s) <- weights
	]
    , "Learning Outcomes" .= A.object 
        [ T.pack n .= ns
	| (n,ns) <- learning 
	]
    , "Grades" .= A.object 
        [ T.pack n .= w
	| (n,w) <- grading
	]
    , "Students" .= toJSON students
    ]

instance FromJSON Class where
  parseJSON = withObject "Class" $ \ v -> Class
    <$> (unKeyValues <$> v .: "Weights")
    <*> (unKeyValues <$> v .: "Learning Outcomes")
    <*> (unKeyValues <$> v .: "Grades")
    <*> v .: "Students"

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

instance FromJSON Student where
  parseJSON = withObject "Students" $ \ o -> case H.toList o of
    [(name,info)] -> (withObject "Students" $ \ i -> Student (T.unpack name) 
         <$> i .: "kuid"
	 <*> i .: "email"
	 <*> i .: "degree"
	 <*> (unKeyValues <$> ((i .: "assignments") <|> pure (KeyValues [])))) info
    _ -> fail "bad student"


newtype Students = Students { unStudents :: [Student] }
newtype AStudent = AStudent (String -> Student)

{-
instance FromJSON AStudent where
   parseJSON = withObject "Students" $ \ o -> AStudent <$> 
     (Student
         <$> o .: "kuid"
	 <*> o .: "email"
	 <*> o .: "degree"
	 <*> (unKeyValues <$> parseJSON (A.Object $ 
	     		      		H.delete "kuid" $
					H.delete "email" $ 
					H.delete "degree" o)))

-}   

instance ToJSON Students where
  toJSON (Students ss) = A.object (map studentToAttribute ss)

{-
instance FromJSON Students where
   parseJSON = withObject "Students" $ \ o -> f <$> parseJSON (A.Object o)
    where
       f :: KeyValues AStudent -> Students
       f (KeyValues kvs) = Students [ g k | (k,AStudent g) <- kvs ]
-}

data Degree = CSBS | CoEBS | EEBS | ICBS | CSBS_CEBS | MathBS | PhysBS | GRAD | CSMS | CoEMS | CSPhD
  deriving (Show, Read)

instance ToJSON Degree where
  toJSON = toJSON . show

instance FromJSON Degree where
  parseJSON a = parseJSON a >>= \ s -> case readMaybe s of
     Just d -> pure d
     Nothing -> fail "bad degree"
  parseJSON other = fail $ show other

data Score = Score Double
     	   | NoScore
  deriving (Show, Eq, Ord)

instance FromJSON Score where
  parseJSON v@A.Number{} = Score <$> parseJSON v
  parseJSON A.Null = pure NoScore
  parseJSON v@A.Array{} = Score . sum . (++ []) <$> parseJSON v 
  parseJSON other = error $ show other

newtype Percent = Percent Double	-- where 100% is Percent 100.0
  deriving Show

instance ToJSON Percent where
  toJSON (Percent n) = toJSON (show n ++ "%")

instance FromJSON Percent where
  parseJSON v = parseJSON v >>= parsePercent
    where
      parsePercent :: Monad m => String -> m Percent
      parsePercent r0 = head $ 
        [ pure (Percent (read p))
        | (p,r1) <- lex r0
        , all isDigit p
        , ("%",r2) <- lex r1
        , ("","") <- lex r2
        ] ++ [fail "bad percent"]

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
  BS.putStrLn $ encodeStudent (Class [] [] [] students)
main2 ["blackboard",code,csvFile,yamlFile] = do
  txt <- BSL.readFile csvFile
  txt <- pure $ case BSL.unpack txt of
    (239:187:191:t) -> BSL.pack t    -- Complete UTF-16 hack
    _               -> txt
  let Right (csv :: V.Vector [String]) = C.decode C.HasHeader txt  
  print csv
  let blackboard :: [(Integer,[String])] = [ s | Just s <- importBlackboard <$> V.toList csv ]
  -- print blackboard
  Right (cls :: Class) <- Y.decodeFileEither yamlFile
  print cls
  BS.putStrLn $ encodeStudent $ injectBlackboard code blackboard cls
main2 ["verify",yamlFile] = do
  r <- Y.decodeFileEither yamlFile
  case r of
   Right a -> print (a :: Class)
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
    order = ["Weights","Learning Outcomes","Grades","kuid","email","degree"]

importBlackboard :: [String] -> Maybe (Integer,[String])
importBlackboard (_:_:_:kuid:_:"Yes":"":_:xs) | all isDigit kuid = pure (read kuid,xs)
importBlackboard xs = error $ show ("blackboard",xs)

injectBlackboard :: String -> [(Integer,[String])] -> Class -> Class
injectBlackboard code env cls@Class{students} = 
  cls { students = injectBlackboardStudent (words code) env <$> students }

injectBlackboardStudent :: [String] -> [(Integer,[String])] -> Student -> Student
injectBlackboardStudent codes env st@Student{name,kuid,assignments} = 
    st { assignments = find <$> sort (nub (map fst assignments ++ codes)) }
  where
   sEnv = case lookup kuid env of
   	    Just es -> zip codes [ case readMaybe (clean e) of
				     Just s -> Score s
				     _ | e == "Needs Grading" -> NoScore
				     _ | e == "" -> NoScore
	    	       	   	     Nothing -> error $ show e
	    	                 | e <-  es 
				 ]
	    Nothing -> error $ "can not find student : " ++ show kuid
   find n = (n,case (lookup n assignments, lookup n sEnv) of
     (Just s,Nothing) -> s
     (Nothing,Just s) -> s
     (Just s1,Just s2) -> if s1 == s2 then s1 else error $ 
     	   	   show ("!",name,kuid,n,s1,s2,sEnv)
     (Nothing,Nothing) -> error "internal error in injectBlackboardStudent")

clean xs | "In Progress(" `isPrefixOf` xs && ")" `isSuffixOf` xs = reverse $ drop 1 $ reverse $ drop 12 xs
         | otherwise = xs
