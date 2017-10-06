{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}

module CCLibPat where

  import Data.List

  import Text.ParserCombinators.Parsec
  import Text.ParserCombinators.Parsec.Char
  import qualified Data.Algorithm.Patience as D
  import Data.List.Split
  --import qualified Diff as D
  import VText
  import Pretty
  import Debug.Trace ( trace )
  --import Data.Vector.Unboxed as Uvector  hiding ((++),concat,length,tail,map,take,drop,concatMap,maximum,reverse)

  escape = "ⱺ"

  data Sel = LSel Int | RSel Int
  type Selection = [Sel]

  data Alt = L | R deriving Eq

  instance Show (Sel) where
    show (LSel d) = (show d) ++ ".l"
    show (RSel d) = (show d) ++ ".r"

  -- Use a sequence of child indices as a path type:
  type Path = [Int]
  type VMap = [(Int, Path)]

  --lift :: [a] -> VText
  --lift = (VText(:[])) . Plain

--  Customized diff
--------------------------------------------------------------------------------

  data Diff a = Same [a] | Different [a] [a] deriving Show
  
  data Edit a = Add a | Delete a | Update a a | NoChange a

  collectDiff :: [D.Item a] -> [Diff a]
  collectDiff [] = []
  collectDiff ((D.Both x _):ds) = append (NoChange x) (collectDiff ds)
  collectDiff ((D.Old x):(D.New y):ds) = append (Update x y) (collectDiff ds) --update
  collectDiff ((D.Old x):ds) = append (Delete x) (collectDiff ds) --delete
  collectDiff ((D.New x):ds) = append (Add x) (collectDiff ds) --insert

  append :: Edit a -> [Diff a] -> [Diff a]
  append (Add x) (Different [] y : ds)       = (Different [] (x:y)) : ds
  append (Delete x) (Different y [] : ds)    = (Different (x:y) []) : ds
  --append (Add x) (Different y [] : ds)       = (Different [] (x:y)) : ds
  --append (Delete x) (Different [] y : ds)    = (Different (x:y) []) : ds
  append (Update x y) (Different x' y' : ds) = (Different (x:x') (y:y')) : ds
  append (NoChange x) (Same y : ds)          = (Same (x:y)) : ds
  append (Add x) ds                          = (Different [] [x]) : ds
  append (Delete x) ds                       = (Different [x] []) : ds
  append (Update x y) ds                     = (Different [x] [y]) : ds
  append (NoChange x) ds                     = (Same [x]) : ds
  

--let v = f k x in v `seq` Tip k v
-- gives the diff output. custom diff so that the 'Both' data contructor can be changed to take only single argument 
--and identify different typed of changes ie., change, delete, insert
  (-?-) :: [String] -> [String] -> [Diff String]
  old -?- new = collectDiff $ D.diff old new


  _diffAsCC :: [Diff Char] -> String
  _diffAsCC [] = ""
  _diffAsCC ((Same x):ds) = x ++ _diffAsCC ds
  _diffAsCC ((Different x y):ds) = concat [
      escape,
      "<",
      x,
      escape,
      ",",
      y,
      escape,
      ">",
      _diffAsCC ds
    ]

  partitionDiff :: Show a => [Diff a] -> [Int] -> [Diff a]
  partitionDiff d bs = let res = p d bs in {-trace ("PartDiff: "++ show res)-} res
    where
      p :: Show a => [Diff a] -> [Int] -> [Diff a]
      p [] _ = []
      p ds [] = ds
      p ds (0:bs) = p ds bs
      p (d:ds) (b:bs) = case d of
        Same x        -> t x x $ flip $ const Same
        Different x y -> t x y Different
        where
          t x y f = {-trace ("Partition Diff "++show x ++ " "++ show y ++ show b)-} (case length x `compare` b of
            EQ -> f x y   : (p ds $ tail bs')
            LT -> f x y   : (p ds bs'')
            GT -> f x' y' : (p (f x'' y'' : ds) $ tail bs'))
            where
              bs' = map (subtract b) $ b:bs
              bs'' = map (subtract $ length x) $ b:bs
              x'  = take b x
              x'' = drop b x
              y'  = take b y
              y'' = drop b y

  diffBoundaries :: [Diff a] -> [Int]
  diffBoundaries [] = []
  diffBoundaries ((Same x):ds) = 0 : (map (+ length x) $ diffBoundaries ds)
  diffBoundaries ((Different x _):ds) = 0 : (map (+ length x) $ diffBoundaries ds)

  diffL :: [Diff a] -> [a]
  diffL [] = []
  diffL ((Same x):ds) = x ++ diffL ds
  diffL ((Different x _):ds) = x ++ diffL ds

  diffR :: [Diff a] -> [a]
  diffR [] = []
  diffR ((Same x):ds) = x ++ diffR ds
  diffR ((Different _ x):ds) = x ++ diffR ds

  showVText :: VText -> String
  showVText (VText []) = ""
  showVText (VText [s]) = showSegment s
  showVText (VText (s:ss)) = (showSegment s) {-++ (if endsWithNewLine' s then "" else " " )-} ++ (showVText (VText ss))
  --showVText (VText ss) = --concatMap showSegment ss

  showSegment :: Segment -> String
  showSegment (Plain t)     = t--showText t--intercalate " " t
  showSegment (Chc d v v') = showChcNoColor d (map showVText [v,v'])
  
  {-showText :: Text -> String
  showText []     = ""
  showText (s:ss) = s ++ showText ss-}
  
  {-showText :: Text -> String
  showText []           = []
  showText ("\n":ss)    = {-"\n" ++ -}showText ss
  showText (s:"\n":ss)  = s {-++ "\n"-} ++ showText ss
  showText [s]          = s
  showText (s:ss)       = s {-++ " " -}++ showText ss-}
  
  {-words' :: String -> [String]
  words' s = case dropWhile (=='\n') s of
    "" -> [] --TODO what if there is just one line?
    s' -> words'' w ++ (if not (null s'') then ["\n"] ++ words' s'' else [])
      where (w,s'') = break (== '\n') s'
  
  {-words' :: String -> [String]
  words' s  =  case dropWhile {-partain:Char.-}(==' ') s of
                                "" -> []
                                s' -> (newLine w) ++ words' s''
                                      where (w, s'') =
                                             break {-partain:Char.-}(== ' ') s'-}
  words'' :: String -> [String]
  words'' " " = [" "]
  words'' s = words s-}
  
  --tokenizes into words and also maintains the spaces between them as tokens.
  tokenizer :: String -> [String]
  tokenizer "" = []
  tokenizer s = 
    let (spaces, s1) = break (/=' ') s
        (lines, s2)  = break (/='\n') s1
        (line, s3)   = break (=='\n') s2
        words        = getwords line
        (nlines,s4)  = break (/= '\n') s3
    in noEmpty spaces ++ noEmpty lines ++ words ++ noEmpty nlines ++ tokenizer s4
    
  getwords :: String -> [String]
  getwords ""    = []
  getwords line  = 
   let (word, s1)   = break (==' ') line
       (spaces, s2) = break (/=' ') s1
   in noEmpty word ++ noEmpty spaces ++ getwords s2
   
  noEmpty :: String -> [String]
  noEmpty "" = []
  noEmpty s  = [s]
  
  {-newLine :: String -> [String]
  newLine s = intersperse "\n" (lines s)
  
  endsWithNewLine :: VText -> Bool
  endsWithNewLine (VText []) = False
  endsWithNewLine (VText vs) = endsWithNewLine' (last vs)
  
  endsWithNewLine' :: Segment -> Bool
  endsWithNewLine' (Plain s) = endsWithNewLine'' s
  endsWithNewLine' (Chc d v1 v2) = endsWithNewLine v1 || endsWithNewLine v2
  
  endsWithNewLine'' :: [String] -> Bool
  endsWithNewLine'' [] = False
  endsWithNewLine'' ss = case (last.last) ss of
   '\n'      -> True
   otherwise -> False-}

--  Parsers
--------------------------------------------------------------------------------

  ccFile :: GenParser Char st VText
  ccFile = do
    doc <- many ccConstruct
    eof
    return $ (VText doc)

  ccConstruct :: GenParser Char st (Segment)
  ccConstruct = do
    try ccChoice <|> ccPlain

  ccPlain :: GenParser Char st (Segment)
  ccPlain = fmap Plain $ many1 $ noneOf escape{-try $ do 
    p <- many1 $ noneOf escape
    trace (show p ++ "AND" ++ show (tokenizer p)) (return $ Plain (tokenizer p))-}
    

  ccChoice :: GenParser Char st (Segment)
  ccChoice = try $ do
    string escape
    dim <- many1 alphaNum
    char '<'
    left <- many ccConstruct
    string escape
    char ','
    right <- many ccConstruct
    string escape
    char '>'
    return $ Chc (read dim :: Int) (VText left) (VText right)

  ccParser :: String -> Either ParseError VText
  ccParser = parse ccFile ""

  dimensions :: VText -> [Int]
  dimensions (VText []) = []
  dimensions (VText ((Plain _):vs)) = dimensions (VText vs)
  dimensions (VText ((Chc d l r):vs)) = let dims = dimensions (VText vs) in dims `seq`
    ([d] ++
      (dimensions l) ++
      (dimensions r) `union`
      dims )
 -- a choice will not have nested choice with dimension same as the outer choice. 
 -- This invariant is applicable in the change commit history scenario only
 -- Therefore `union` can be replaced with simple concatenation

  nextDimension :: VText -> Int
  nextDimension = succ . maximum . dimensions

  latest :: VText -> [Sel]
  latest = map RSel . dimensions
---------------------------------------------------------------------------------
  ppVText :: VText -> String
  ppVText (VText xs) = concatMap show xs

  applySelectionWithMap :: Selection-> VText -> (Text, VMap)
  applySelectionWithMap s = a [0] 0
    where
      a :: Path -> Int -> VText -> (Text, VMap)
      a _ _ (VText []) = ([], [])
      a p o (VText((Plain b):vs)) =
        (b, (o, reverse p)) ^: a (psucc p) (o + length (tokenizer b)) (VText vs)
      a p o (VText ((Chc d l r):vs)) =
        a' ^++ a (psucc p) (o + length (tokenizer v'))(VText vs)
        where
          c = case d `asSelectedIn` s of { L -> l; R -> r }
          a'@(v', _) = a (0:p) o c

      (^:) :: (Text, (Int, Path)) -> (Text, VMap) -> (Text, VMap)
      (v, p) ^: (vs, ps) = (v ++ vs, p:ps)

      (^++) :: (Text, VMap) -> (Text, VMap) -> (Text, VMap)
      (v, p) ^++ (vs, ps) = (v ++ vs, p ++ ps)

      psucc (p:ps) = succ p : ps

  applySelection :: [Sel] -> VText -> Text
  applySelection s v = fst $ applySelectionWithMap s v

  asSelectedIn :: Int -> [Sel] -> Alt
  asSelectedIn d ((LSel d'):ss) = if d == d' then L else d `asSelectedIn` ss
  asSelectedIn d ((RSel d'):ss) = if d == d' then R else d `asSelectedIn` ss


  denormalizeV ::Selection -> VText -> [Int] -> VText
  denormalizeV s v b = fst $ d s v b --let x = d s v b in x `seq` fst $ x
    where
      (^:) :: Segment -> (VText, [Int]) -> (VText, [Int])
      x ^: (VText y, z) = (VText(x:y), z)

      d :: Selection -> VText -> [Int] -> (VText, [Int])
      d _ (VText []) bs = (VText [], bs)
      d _ vs [] = (vs, [])
      d s vs (0:bs) = d s vs bs
      d s (VText((Chc dim l r):vs)) bs = case dim `asSelectedIn` s of
        L -> e id   l r
        R -> e flip r l
        where
          e f x y = ((f $ Chc dim) x' y) ^: z
            where
              (x', bs') = d s x bs
              z = (d s (VText vs) bs')
      d s (VText((Plain x):vs)) (b:bs) = {-trace ("Denormalize: " ++ show x ++ " | " ++ show b )-}(case length (tokenizer x) `compare` b of
        EQ -> (Plain x) ^: z
        LT -> (Plain x) ^: (d s (VText vs) bs'')
        GT -> (Plain (concat x')) ^: z')
        where
          bs1'   = map (subtract b) $! b:bs
          bs''  = map (subtract $ length (tokenizer x)) (b:bs)
          x'    = take b (tokenizer x)
          x''   = drop b (tokenizer x)
          z     = (d s (VText vs) $ tail bs1')
          z'    = bs1' `seq` (d s (VText((Plain $ (concat $ x'')):vs)) $! tail bs1')

  normalize :: VText -> VText
  normalize (VText []) = VText []
  normalize (VText ((Plain x):(Plain y):z)) = normalize $ VText (Plain (x ++ y) : z)
  normalize (VText (x@(Plain _):y)) = case (normalize (VText y)) of VText ys -> VText (x:ys)
  normalize (VText ((Chc d (VText l) (VText r)):(Chc d' (VText l') (VText r')):z)) = if d == d'
    then  normalize $ VText (Chc d (normalize $ (VText (l ++ l'))) (normalize $ (VText (r ++ r'))) : z)
    else  VText ((Chc d (normalize (VText l)) (normalize (VText r))) :
            (Chc d' (normalize (VText l')) (normalize (VText r'))) : (case (normalize (VText z)) of VText zs -> zs) )
  normalize (VText ((Chc d l r):z)) =
    VText (Chc d (normalize l) (normalize r) : (case (normalize (VText z)) of VText zs -> zs) )

--Unnesting
-----------------------------------------------------------------------------------------
{-unnest :: VText -> VText
unnest VText [] = VText []
unnest VText xs = unnestRecur xs

unnestRecur :: [Segment] -> [Segment]
unnestRecur [] = []
unnestRecur (x:xs) = case x of
                      Plain a -> [Plain a] ++ (unnestRecur xs)
                      c -> (unnestChoices c) ++ (unnestRecur xs)

unnestChoices :: Segment -> [Segment]
unnextChoices [] = []
unnestChoices Chc d v1 (VText vs) = case (last vs) of
			             Chc d1 (VText [Plain ""]) (VText [Plain a]) -> (unnestChoices $ Chc d v1 (VText (init vs))) ++ [Chc d1 (VText [Plain ""]) (VText [Plain a])]
                                     _ -> [Chc d v1 (VText vs]
unnestChoices (Plain a) = [Plain a]

sepSegments :: [Segment] -> [Segment]
sepSegments [] = []
sepSegments xs = case last xs of --last takes O(n). Use sequence which has constant time access to the fisrt and last element
                  Plain x -> xs
                  Chc d
 -}


--  Distillation
--------------------------------------------------------------------------------

  distill :: Int -> VText -> Selection -> Text -> VText
  distill dim v s n = normalize $ fst $ l pv d
    where
      (o, m) = s `applySelectionWithMap` v
      d = {-trace ("Old: "++ show o ++ ",tokenized: "++ show (tokenizer o) ++ ", New: " ++ show n ++ ",tokenized:" ++ show (tokenizer n) ++ "VMap" ++ show m)-} (partitionDiff ((tokenizer o) -?- (tokenizer n)) $ map fst m)
      pv = {-trace ("VText: "++show v)-} (denormalizeV s v $ diffBoundaries d)

      (^:) :: Segment-> (VText, [Diff String]) -> (VText, [Diff String])
      x ^: (VText y, z) = (VText (x:y), z)

      l :: VText -> [Diff String] -> (VText, [Diff String])

      l (VText []) di@((Different [] x):ds) = {-trace ("1. [] and "++ show di)-} (VText [Chc dim (VText []) (VText [Plain (concat x)])], ds)

      l (VText []) ds = (VText [], ds)

      -- Unchanged plain text:
      l vt@(VText ((Plain x):vs)) dif@((Same _):ds) = {-trace ("2."++ show vt ++ " and "++ show dif)-} (Plain x ^: (l (VText vs) ds))

      -- Addition:
      l vs dif@((Different [] x):ds) = {-trace ("3."++ show vs ++ " and "++ show dif)-} (Chc dim (VText []) (VText [Plain (concat x)]) ^: (l vs ds))

      -- Removal:
      l vt@(VText ((Plain x):vs)) dif@((Different x' []):ds) = {-trace ("4."++ show vt ++ " and "++ show dif)-} 
        (Chc dim (VText [Plain x]) (VText []) ^: (l (VText vs) ds))

      -- Changed plain:
      l vt@(VText ((Plain x):vs)) dif@((Different x' y):ds) = {-trace ("5."++ show vt ++ " and "++ show dif)-} (if x == (concat x')
        then Chc dim (VText [Plain x]) (VText [Plain (concat y)]) ^: (l (VText vs) ds)
        else error $ concat [ "Mismatch ", show vt, " /= ", show x', " and ", show y ])

      -- Recurse downward along choice:
      l vt@(VText ((Chc dim' left right):vs)) ds = {-trace ("6."++ show vt ++ " and "++ show ds)-} (case dim' `asSelectedIn` s of
        L -> l' id    left right
        R -> l' flip  right left)
        where
          l' f x y = ((f $ Chc dim') x' y) ^: (l (VText vs) ds')
            where
              (x', ds') = l x ds
      
      l vs ds = trace (show vs ++ " and " ++ show ds) undefined
