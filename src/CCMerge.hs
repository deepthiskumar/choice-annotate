module CCMerge where

{-
 For merging 2 Choice calculus expressions, both the expressions need to be of similar structure.
 That is to say, changed/unchanged characters in the mergee file should correspond to the same changed/unchanged characters in the base file.
 (Mergee is being merged into the base)
 For example, abcde --> 1<ab,xy>cde (mergee)
                    --> 1<a,m>bcde (base)
  Here the choice expressions in mergee and base represent changes on different Strings, one for "ab" and the other for "a" .
  For these to be compared, Choice in mergee needs to be split into 2 choices => 1<a,x>1<b,y>cde.
  Now update on character "a" can be easily merged.
  But edit on character b cannot be easily compared because base has the whole of bcde unchanged.
  Hence we split Plain "bcde" to Plain "b" and Plain "cde".
  Now changed b in mergee can be compared with unchanged b in base. Both expressions have "cde" unchanged and therefore is simply propogated.

  The dimensions in mergee are renamed by so that when they are merged, the dimensions don't conflict.
  Example 1 (new base is the merged CC expresion)
  base     : 1<ab,ij>
  mergee   : 1<ab,x2<y,l>> => 2<ab,x3<y,l>>
  new base : 1<2<ab,x3<y,l>>,ij>

  Example 2 (common dimension: 1)
  base     : 1<ab,2<x,r>y>
  mergee   : 1<ab,x2<y,l>> => 1<ab,x3<y,l>>
  new base : 1<ab,2<x,r>3<y,l>>

 The Algorithm for merging is as follows:
 Given -  1. Common dimension from which the branch was created

 1. Split all the choices and plain expressions at the character level in both mergee and base
    Example : Plain "ab" => Plain "a",Plain "b"
              Chc 1 (plain "ab") (plain "xy") => Chc 1 (plain "a") (plain "x"), Chc 1 (plain "b") (plain "y")
 2. rename the dimensions in mergee
 3. Merge both the CC Expressions
 4. Unify the expressions where possible (Opposite of split)

-}


--import VText
import Data.List as L
import CCLibPat
import Pretty
import Data.Text as T
import qualified Data.Algorithm.Diff as D
import Data.Algorithm.Patience as PatDiff
import Debug.Trace

type DimVText = (VString,Dim)
type DimSegment = (Segment,Dim)


data Action = Copy VString | Compare VString VString
   deriving(Show)


plain :: String -> VString
plain s = [Str $ T.pack s]


--Apply partial decision (i.e., not all the decisions in a view decision are applied).
--The output will be a VText where applied decisions will be a Plain a and the remaining decisions will remain as Choices
--in order to get a choice expression up till a particular commit by selecting left choice for all the dimensions that come after it
partialDecision :: Selection -> VString -> VString
partialDecision [] []                     = []
partialDecision [] v                      = v
partialDecision ss []                     = []
partialDecision (ss) (Str a :vs)          = [Str a] `appendVTexts` partialDecision ss vs
partialDecision (ss) (c@(Chc d l r) : vs) = case getSelection d ss of 
 Just L  -> applyDecision ss l `appendVTexts` partialDecision ss vs
 Just R  -> applyDecision ss r `appendVTexts` partialDecision ss vs
 Nothing -> [Chc d (partialDecision ss l) (partialDecision ss r)]   `appendVTexts` partialDecision ss vs

applyDecision :: Selection -> VString -> VString
applyDecision ss p@([ Str a]) = p
applyDecision ss v             = partialDecision ss v

appendVTexts :: VString -> VString -> VString
appendVTexts = (++)
{-appendVTexts [] v           = v
appendVTexts v          (VText [])  = v
appendVTexts (VText ss) (VText ss') = VText (ss ++ ss' )-}

getSelection :: Int -> [Sel] -> Maybe Alt
getSelection d []             = Nothing
getSelection d ((LSel d'):ss) = if d == d' then Just L else d `getSelection` ss
getSelection d ((RSel d'):ss) = if d == d' then Just R else d `getSelection` ss


-- | tests for partial view decision
--
-- >>>showVText $ partialDecision  [RSel 1,RSel 2] ( [Chc 1 (plain "a") (plain "x"),Chc 2 (plain "b") (plain "y") ])
-- "xy"
-- >>>showVText $ partialDecision  [RSel 1] ( [Chc 1 (plain "a") (plain "x"),Chc 2 (plain "b") (plain "y") ])
-- "x2<b,y>"
-- >>>showVText $ partialDecision  [RSel 1] ( [Chc 1 (plain "a") ([Chc 3 (plain "x") (plain "z")]),Chc 2 (plain "b") (plain "y") ])
-- "3<x,z>2<b,y>"
-- >>>showVText $ partialDecision  [LSel 1] ( [Chc 1 (plain "a") ([Chc 3 (plain "x") (plain "z")]),Chc 2 (plain "b") (plain "y") ])
-- "a2<b,y>"
-- >>>showVText $ partialDecision  [LSel 1] ( [str "m",Chc 1 (plain "a") ([Chc 3 (plain "x") (plain "z")]),str "n",Chc 2 (plain "b") (plain "y") ])
-- "man2<b,y>"
-- >>>showVText $ partialDecision  [RSel 1,RSel 2] ( [str "m",Chc 1 (plain "a") ([Chc 3 (plain "x") (plain "z")]),str "n",Chc 2 (plain "b") (plain "y")]) 
-- "m3<x,z>ny"
-- >>>showVText $ partialDecision  [RSel 3,RSel 2] ( [str "m",Chc 1 (plain "a") ([Chc 3 (plain "x") (plain "z")]),str "n",Chc 2 (plain "b") (plain "y")])
-- "m1<a,z>ny"

---------------------------------------------------------------------------------------
{-To identify if the actual changes made by the user in the merge commit corresponds 
  to the changes present in either of the branches.
  The function takes 1) merged VText: ccmerge between both the branches before taking the merged commit
                     2) next VText: VText after applying the merged changes to its CC exp (the one without the merge )
                       (The dimensions added in 'next VText' will be inline with the merged dimensions and therefore
                       no overlapping of dimensions. Also next VText will have only one level of nesting atmost since it has only 1 change)
  selections need to be merged seperately
-}
patchCC :: VString -> VString -> (VString,Selection)
patchCC m n = let m' = agressiveSplit m
                  n' = agressiveSplit n
              in compareChanges m' n'

compareChanges :: VString -> VString -> (VString,Selection)
compareChanges (m:ms) (n:ns) 
   | m == n    = case compareChanges ms ns of (v,ss) -> ([m] `appendVTexts` v, ss)
   | otherwise = compareChanges' m n ms ns
   
   {-case (m,n) of
     (Str a, Str b)                    -> if a == b then case compareChanges ms ns of (v,ss) -> ([Str a] `appendVTexts` v, ss) else undefined --if plain then both should be same
     (c1@(Chc d1 [Str ""] r1),c2@(Chc d2 [Str ""] r2)) -> case compareChanges (m:ms) ns of (v,ss) -> ([Chc d1 (plain "") [Chc d2 r1 r2]] `appendVTexts` v, (RSel d1):ss)
     (_, c@(Chc d [Str ""] r))    -> case compareChanges (m:ms) ns of (v,ss) -> ([c] `appendVTexts` v, ss) --check if the a in plain is equal to the one in l to make sure its the corresponding
     
     (c@(Chc d [Str ""] r), _)    -> case compareChanges ms (n:ns) of (v,ss) -> ([c] `appendVTexts` v, (LSel d) : ss)
     
     (Str a, c@(Chc d l r))              -> case compareChanges ms ns of (v,ss) -> ([c] `appendVTexts` v, ss) --l should have a, get selection from nextvText
     (c@(Chc d l r), Str a)              -> case compareChanges ms ns of (v,ss) -> ([c] `appendVTexts` v, ss) --l should have a, get selection from merge
     (c1@(Chc d1 l1 r1),c2@(Chc d2 l2 r2)) -> case d1 == d2 of
       True   -> case (compareChanges l1 l2, compareChanges r1 r2) of
                  ((v1,ss1),(v2,ss2)) -> case compareChanges ms ns of (v,ss) -> ([Chc d1 v1 v2] `appendVTexts` v, ss) 
       False  -> case l1 == l2 of --something different added by the user since if L is same but R is diff otherwise same as one of the branches (which is handled by above patterns)
        True -> case (r1,r2) of
         ([Str a],[Str b]) -> case compareChanges ms ns of (v,ss) -> ([Chc d1 l1 ([Chc d2 [Str a] [Str b]])] `appendVTexts` v, (RSel d1) : ss)
         (v1,v2)                         -> let (r',ss') = compareChanges v1 v2 in case compareChanges ms ns of (v,ss) -> ([Chc d1 l1 (r')] `appendVTexts` v, ss'++ss)
        False -> undefined --something is wrong if both d's and l's are different-}
        

compareChanges' :: Segment -> Segment -> VString -> VString -> (VString,Selection)
compareChanges' (Str a) (Str b) ms ns
   | a == b     = let (v,ss) = compareChanges ms ns in ([Str a] `appendVTexts` v, ss)
compareChanges' m@(Chc d1 [Str s] r1) n@(Chc d2 [Str s'] r2) ms ns
   | T.length s == 0 && T.length s' == 0 = let (v,ss) = compareChanges (m:ms) ns in ([Chc d1 (plain "") [Chc d2 r1 r2]] `appendVTexts` v, (RSel d1):ss)
compareChanges' m n@(Chc d [Str s] r) ms ns
   | T.length s == 0  = let (v,ss) = compareChanges (m:ms) ns in ([n] `appendVTexts` v, ss)
compareChanges' m@(Chc d [Str s] r) n ms ns
   | T.length s == 0 = let (v,ss) = compareChanges ms (n:ns) in ([m] `appendVTexts` v, (LSel d) : ss)
compareChanges' (Str a) c@(Chc d l r) ms ns = let (v,ss) = compareChanges ms ns in ([c] `appendVTexts` v, ss) --l should have a, get selection from nextvText
compareChanges' c@(Chc d l r) (Str a) ms ns = let (v,ss) = compareChanges ms ns in ([c] `appendVTexts` v, ss) --l should have a, get selection from merge
compareChanges' c1@(Chc d1 l1 r1) c2@(Chc d2 l2 r2) ms ns 
   | d1 == d2   = let (v1,ss1) = compareChanges l1 l2
                      (v2,ss2) = compareChanges r1 r2
                      (v,ss)   = compareChanges ms ns
                  in ([Chc d1 v1 v2] `appendVTexts` v, ss)
   | l1 == l2   = case (r1,r2) of  --something different added by the user since if L is same but R is diff otherwise same as one of the branches (which is handled by above patterns)
                   ([Str a], [Str b]) ->  let (v,ss) = compareChanges ms ns
                                          in ([Chc d1 l1 ([Chc d2 [Str a] [Str b]])] `appendVTexts` v, (RSel d1) : ss)
                   (v1,v2)            ->  let (r',ss') = compareChanges v1 v2
                                              (v,ss)   = compareChanges ms ns
                                          in ([Chc d1 l1 (r')] `appendVTexts` v, ss'++ss)
   | otherwise  = undefined --something is wrong if both d's and l's are different

--Done
--split the plain string into characters
granulatePlain :: [Segment] -> [Segment]
granulatePlain [] = []
granulatePlain ((Str a) : xs)   = (getPlainTokens a) ++ granulatePlain xs
granulatePlain ((Chc d l r : xs)) = (Chc d (granulatePlain l) (granulatePlain r)) : granulatePlain xs

--Done
getPlainTokens :: Text -> VString
getPlainTokens s 
  | T.length s == 0 = [Str T.empty]
  | otherwise       = L.map Str (tokenizer s) 

{-getPlainChar :: Text -> [Segment]
getPlainChar s 
  | T.length s == 0 = [Str T.empty]
  | T.length s == 1 = [Str s]
  | otherwise       = (Str $ T.singleton $ T.head s) : (getPlainChar $ T.tail s)-}

-- |
-- >>> showVText $ vSplit ( [str "abc"])
-- "abc"
--
-- >>> showVText $ vSplit ( [str "ab lm", Chc 1 ([str "c"]) ([str "x"])])
-- "ab lm1<c,x>"
--
-- >>> showVText $ vSplit ( [str "a", Chc 1 ([str "b"]) ([str "y"])])
-- "a1<b,y>"
--
-- >>> showVText $ vSplit ( [Chc 1 (plain "ab ij") ([Chc 2 (plain "xy jk") (plain "zl op"),str "y m n"]),str "cr ty"])
-- "1<ab,2<xy,zl>>1< ,2< , >>1<ij,2<jk,op>>1<,y>1<, >1<,m>1<, >1<,n>cr ty"
--
-- >>> showVText $ vSplit ( [Chc 1 ([Chc 2 (plain "xy jk") (plain "zl op"),str "ym n"]) (plain "ab op"),str "crty"])
-- "1<2<xy,zl>,ab>1<2< , >, >1<2<jk,op>,op>1<ym,>1< ,>1<n,>crty"
--
-- >>> showVText $ vSplit ( [Chc 1 ([Chc 2 (plain "xy jk") (plain "zl"),str "ym n"]) (plain "ab op"),str "crty"])
-- "1<2<xy,zl>,ab>1<2< ,>, >1<2<jk,>,op>1<ym,>1< ,>1<n,>crty"
--
-- >>> showVText $ vSplit ( [Chc 1 (plain "ab ij") ([Chc 2 (plain "xy jk") (plain "zl"),str "y m n"]),str "cr ty"])
-- "1<ab,2<xy,zl>>1< ,2< ,>>1<ij,2<jk,>>1<,y>1<, >1<,m>1<, >1<,n>cr ty"
-- >>> showVText $ vSplit ([Chc 1 (plain "x y") [Chc 2 (plain "") (plain "a "), str"l"]])
-- "1<,2<,a>>1<,2<, >>1<x,l>1< ,>1<y,>"
-- >>> showVText $ vSplit ([Chc 1 ([str "x ",Chc 3 (plain "") (plain"b "), str "y"]) [Chc 2 (plain "") (plain "a "), str"l"]])
-- "1<,2<,a>>1<,2<, >>1<x,l>1< ,>1<3<,b>,>1<3<, >,>1<y,>"
-- >>> showVText $ vSplit ([Chc 1 ([str "x\n",Chc 3 (plain "") (plain"b "), str "y"]) [str "l m"]])
-- "1<x,l>1<\n, >1<3<,b>,>1<3<, >,>1<y,m>"


--Done
vSplit::VString -> VString
vSplit x = agressiveSplit (granulatePlain x)

--Done
--Splits the choice expression at character level
agressiveSplit :: VString -> VString
agressiveSplit []     = []
agressiveSplit (x:xs) = let ys = splitSegment x
                            zs = agressiveSplit xs
                        in (ys ++ zs)
--Done
splitSegment :: Segment -> [Segment]
splitSegment (Str x) = [Str x]
splitSegment (Chc d l r) = case (l,r) of
                            ([],[])                     -> []
                            ([],ys)                     -> splitSegment (Chc d (plain "") ys )
                            (xs,[])                     -> splitSegment (Chc d xs (plain ""))
                            (Str x : xs,Str y : ys) -> (Chc d [Str x] [Str y]) : (splitSegment (Chc d xs ys))
                            (x : xs, y : ys)            -> let (c,xs',ys') =  splitSegmentInsert d (splitSegment x) (splitSegment y) 
                                                           in (c) : splitSegment (Chc d (xs'++xs) (ys'++ys))
                                                           
splitSegmentInsert :: Dim -> [Segment] -> [Segment] -> (Segment,[Segment],[Segment])
splitSegmentInsert d ((Str a):ls) ((Str b):rs) = ((Chc d [Str a] [Str b]), ls, rs) 
splitSegmentInsert d ((Str a):ls) (r:rs)
    | isInsert r && (not $ T.null a) = ((Chc d (plain "") [r]),((Str a):ls), rs)
    | otherwise  = ((Chc d [Str a] [r]), ls, rs)  
splitSegmentInsert d (l:ls) ((Str b):rs)
    | isInsert l && (not $ T.null b)= ((Chc d [l] (plain "")), ls, ((Str b):rs)) 
    | otherwise  = ((Chc d [l] [(Str b)]), ls, rs)
splitSegmentInsert d (l:ls) (r:rs)
--    | isInsert l && isInsert r = ((Chc d [l] [r]), ls, rs) Should still be stored separately
    | isInsert r = ((Chc d (plain "") [r]),(l:ls), rs)
    | isInsert l = ((Chc d [l] (plain "")), ls, (r:rs))
    | otherwise  = ((Chc d [l] [r]), ls, rs)                                                        

unifyVText :: VString -> VString
unifyVText []       = []
unifyVText [s]      = case s of
                        Str _   -> [s]
                        Chc d l r -> [Chc d (unifyVText l) (unifyVText r)]
{-unifyVText (s:t:ss) = case unifySegment s t of
                                  Just u  -> unifyVText (u:ss)
                                  Nothing -> let us = unifyVText (t:ss)
                                             in (s:us)-}
unifyVText (s:t:ss) = case (unifyVText [s], unifyVText [t]) of
                        ([Str x], [Str y]) -> unifyVText ((Str (T.append x y)) : ss)
                        ([s'@(Str x)], [t'@(Chc _ _ _ )]) -> s': unifyVText (t':ss)
                        ([s'@(Chc _ _ _ )], [t'@(Str x)]) -> s': unifyVText (t':ss)
                        ([s'@(Chc d l r )], [t'@(Chc d' l' r' )]) -> 
                           case d == d' of
                             True  -> unifyVText (Chc d (unifyVText (l++l')) (unifyVText (r ++ r')) : ss)
                             False -> s' : unifyVText (t':ss) 
                        otherwise    -> error "Error unifying the choices"


--unify the two segment, if same dimension return unified else return both of them
unifySegment :: Segment -> Segment -> Maybe Segment
unifySegment (Chc d l r) (Chc d' l' r')
                   |  d == d'     = Just $ Chc d (unifyVText (l++l')) (unifyVText (r ++ r'))
                   | otherwise    = Nothing
unifySegment (Str x) (Str y)  = Just (Str (T.append x y))
unifySegment _ _                  = Nothing

--increment all the dimensions by a value so that they dont collide with the dimensions in the expression on to which it is being merged
-- it takes the latest dimension and also the latest common dimension between the 2 expressions
renameDimensions :: Int -> Int -> VString -> VString
renameDimensions ld cd []     = []
renameDimensions ld cd (x:xs) = let s = renameDimension ld cd x
                                    vs = renameDimensions ld cd xs
                                in (s : vs)

renameDimension :: Int -> Int -> Segment -> Segment
renameDimension _ _ (Str a)      = (Str a)
renameDimension ld cd (Chc d l r)
       | d > cd                    = (Chc (d + (ld-cd)) (renameDimensions ld cd l) (renameDimensions ld cd r))
       | otherwise                 = (Chc d (renameDimensions ld cd l) (renameDimensions ld cd r))


--mergee, base and merged base
mergeVText :: VString -> VString -> VString
mergeVText [] (ds)       = (ds)
mergeVText (ss) []       = (ss)
mergeVText (s:ss) (d:ds) = let (seg, (l, r)) =trace ("MergevText L:" ++ show (s:(gethead ss)) ++"\nMergevText R:" ++show (d:(gethead ds))) (mergeSegment s d)--mergeChoices s d ss ds
                           in case (l,r) of
                             (Nothing, Nothing) -> seg : mergeVText ss ds
                             (Just x, Nothing)  -> seg : mergeVText (x:ss) ds
                             (Nothing, Just y)  -> seg : mergeVText ss (y:ds)
                             (Just x, Just y)   -> seg : mergeVText (x:ss) (y:ds)
                             
gethead :: [a] -> [a]
gethead [] = []
gethead (a:as) = [a]
                                                                                        
{-mergeChoices :: Segment -> Segment -> VString -> VString -> VString
mergeChoices s@(Chc x [Str s'] r) d@(Chc y [Str d'] r') ss ds
  | T.null s' && T.null d' = let d' = mergeSegment s d
                                 ds' = mergeVText ss ds
                             in (d' : ds')
mergeChoices c@(Chc x [Str s'] r) d ss ds 
  | T.null s'  = let ds' = mergeVText ss (d:ds)
                 in (c : ds')
mergeChoices s c@(Chc y [Str s'] r) ss ds 
  | T.null s'   = let ds' = mergeVText (s:ss) ds
                  in (c : ds')
mergeChoices s d ss ds = let d' = mergeSegment s d
                             ds' = mergeVText ss ds
                         in (d' : ds') -}                                                       


mergeSegment :: Segment -> Segment -> (Segment, (Maybe Segment, Maybe Segment))
mergeSegment x@(Str a) y@(Str b)
    | a == b                             = (x,(Nothing,Nothing))
    | otherwise                          = trace ("Case 1: "++show x ++ "|" ++ show y) undefined--(x,(Nothing,Just y))
mergeSegment x@(Chc d l r) y@(Str a)
    | isInsert x && T.null a             = (x, (Nothing, Nothing))
    | isInsert x                         = (x, (Nothing, Just y))        
    | a `isIn` (L.head l)                = (x, (Nothing, Nothing)) --delete or update
    | otherwise                          = trace ("See: "++show x ++ "|" ++ show y) undefined--(x,(Nothing,Just y))
mergeSegment x@(Str a) y@(Chc d' l' r')   
    | isInsert y && T.null a             = (y, (Nothing, Nothing))
    | isInsert y                         = (y, (Just x, Nothing))
    | a `isIn` (L.head l')               = (y, (Nothing, Nothing))  
    | otherwise                          = trace ("See: "++show x ++ "|" ++ show y) undefined --(y,(Just x,Nothing))
mergeSegment x@(Chc d l r) y@(Chc d' l' r')
    | x == y             = (x, (Nothing,Nothing)) 
    | d == d' && l == l' && isInsert (L.head r) && isInsert (L.head r') = let (seg,(segs,segd)) = mergeSegment (L.head r) (L.head r')
       in (Chc d' l [seg],(maybe Nothing (\v -> Just $ Chc d l [v]) segs, maybe Nothing (\v -> Just $ Chc d l [v]) segd))
    | d == d' && l == l' && isInsert (L.head r) = (x, (Nothing, Just y))
    | d == d' && l == l' && isInsert (L.head r') = (y, (Just x, Nothing))
    | d == d' && l == l'  = let (seg,(segs, segd)) = mergeSegment (L.head r) (L.head r')
                            in (Chc d l [seg], (maybe Nothing (\v -> Just $ Chc d l [v]) segs,
                              maybe Nothing (\v -> Just $ Chc d l [v]) segd))
    | d == d' && r == r' && isInsert (L.head l) && isInsert (L.head l') = let (seg,(segs, segd)) = mergeSegment (L.head l) (L.head l')
        in (Chc d [seg] r, (maybe Nothing (\v -> Just $ Chc d [v] r) segs,
                              maybe Nothing (\v -> Just $ Chc d [v] r) segd))
    | d == d' && r == r' && isInsert (L.head l) = (x, (Nothing, Just y))
    | d == d' && r == r' && isInsert (L.head l') = (y, (Just x, Nothing))
    | d == d' && r == r'  = let (seg,(segs, segd)) = mergeSegment (L.head l) (L.head l')
                            in (Chc d [seg] r, (maybe Nothing (\v -> Just $ Chc d [v] r) segs,
                              maybe Nothing (\v -> Just $ Chc d [v] r) segd))
    | d == d' && isInsert x = (x, (Nothing, Just y))
    | d == d' && isInsert y = (y, (Just x, Nothing))
    | d == d'               = let (seg,(segs, segd)) = mergeSegment (L.head l) (L.head l')
                                  (seg',(segs', segd')) = mergeSegment (L.head r) (L.head r')
                              in (Chc d [seg] [seg'], (makeSeg d segs segs',makeSeg d segd segd'))
    | d /= d' && isInsert x && isInsert y = (branchInto x y, (Nothing,Nothing))
    | d /= d' && isInsert x  = (x, (Nothing, Just y))
    | d /= d' && isInsert y  = (y, (Just x, Nothing))
    | d /= d' && x `hasOrIn` y = (branchInto x y, (Nothing,Nothing))
--mergeSegment x y             = trace ("See: "++show x ++ "|" ++ show y) undefined                       
--    | otherwise

makeSeg :: Dim -> Maybe Segment -> Maybe Segment -> Maybe Segment
makeSeg d (Just x) (Just y) = Just (Chc d [x] [y])
makeSeg _ _ _               = Nothing

branchInto :: Segment -> Segment -> Segment
branchInto x (Chc d [Str a] r) = Chc d [x] r
branchInto x (Chc d [y@(Chc _ _ _)] r) = Chc d [branchInto x y] r  

isInsert :: Segment -> Bool
isInsert (Str _)     = False
isInsert (Chc _ l _) = 
   case L.head l of
     Str x         -> T.null x
     c@(Chc _ _ _) -> isInsert c

hasOrIn :: Segment -> Segment -> Bool
hasOrIn (Chc d [Str a] r) (Chc d' [Str b] r')
    | a == b = True
hasOrIn (Chc d [Str a] r) (Chc d' l' r')
    | a `isIn` (L.head l') = True
hasOrIn (Chc d l r) (Chc d' [Str b] r')
    | b `isIn` (L.head l)  = True
hasOrIn (Chc d l r) (Chc d' l' r') = hasOrIn (L.head l) (L.head l')
hasOrIn _ _ = False
          

isIn :: Text -> Segment -> Bool
isIn x (Str y)     = x == y
isIn x (Chc d l r) = isIn x (L.head l)  


{-mergeSegment :: Segment -> Segment -> Segment
mergeSegment (Str a) (Str b)        = (Str b) --Invariant 1 If a character in the same position is different in both the files, then atleast either one has to be a choice
mergeSegment (Chc d l r) (Str b)      = (Chc d l r) --what if l /= b
mergeSegment (Str a) (Chc d l r)      = (Chc d l r) -- what if a /= l
mergeSegment c1@(Chc d l r) c2@(Chc d' l' r')
   | c1 == c2 = c2
   | otherwise = mergeChoice c1 c2
-}
{-mergeChoice :: Segment -> Segment -> Segment
mergeChoice c1@(Chc d l r) c2@(Chc d' l' r')
  | d == d'   = case l == l' of --left or right alternative has changes
                 True  -> case r == r' of
                           True  -> c2
                           False -> Chc d l' (mergeVText r r')
                 False -> case r == r' of
                           True  -> Chc d (mergeVText l l') r'
                           False -> undefined --TODO ** Chc d' [Chc d (mergeVText l l') r] r' Not even when the branches have inner branching? I guess no
                           --undefined --Chc d (mergeVText l l') (mergeVText r r') --Invariant 2. This is not allowed (an invariant we impose). A choice can have edits either in left or right alternatives. Not both
  | otherwise = (Chc d' [(mergeSegment c1 (L.head l'))] r')-- these are new choices introduced and put c1 in the left branch of c2
-}

maxDimensionS :: VString -> Int
maxDimensionS vt = case dimensions vt of
  [] -> 1
  ds -> (L.maximum ds)

--Common dimension (Needs to be stored while branching out)
mergeCC :: Int -> DimVText -> DimVText -> DimVText
mergeCC cd (vs,d) (vs',d') = let as'= renameDimensions d' cd (vSplit vs)
                                 rs'= unifyVText $ mergeVText (as') (vSplit vs')
                             in  (rs', max (maxDimensionS as') d')
                             
mergeCCNew :: Int -> DimVText -> DimVText -> DimVText
mergeCCNew cd (vs,d) (vs',d') = let rs'= unifyVText $ mergeVText (vSplit vs) (vSplit vs')
                                in (rs', max d d')
                                
mergeVS :: (Selection,VString) -> (Selection,VString) -> (Selection,VString)
mergeVS (s,vs) (s',vs') = ((nub s) `union` s', unifyVText $ mergeVText (vSplit vs) (vSplit vs'))

mergeDiffVS :: VString -> VString -> VString
mergeDiffVS vs vs' = processActions $ actions vs vs'

processActions :: [Action] -> VString
processActions [] = []
processActions (Copy vs : as) = vs ++ processActions as
processActions (Compare vs vs' : as) = (mergeVText vs vs') ++ processActions as  
                                

---------find longest common subsequence between both the vstrings-------------
--Split VStrings
actions :: VString -> VString -> [Action]
actions vs vs' = processPatDiff $ PatDiff.diff vs vs'--D.getGroupedDiff vs vs'


processPatDiff :: [PatDiff.Item Segment] -> [Action]
processPatDiff [] = []
processPatDiff ((PatDiff.Old s) : ds) = 
  let (olds, rem) = collectOld ds
      (news, rem') = collectNew rem
  in (Compare (s:olds) news) : processPatDiff rem'
processPatDiff ((PatDiff.New s) : ds) = 
  let (news, rem) = collectNew ds
  in (Compare [] (s:news)) : processPatDiff rem
processPatDiff (PatDiff.Both s _ : ds) = 
  let (both, rem) = collectBoth ds
  in (Copy (s:both)) : processPatDiff rem

collectOld :: [PatDiff.Item Segment] -> (VString,[PatDiff.Item Segment])
collectOld []           = ([],[])
collectOld ((PatDiff.Old s) : is) = 
  let p = collectOld is
  in  (s : (fst p), snd $ p)
collectOld is            = ([],is)

collectNew :: [PatDiff.Item Segment] -> (VString,[PatDiff.Item Segment])
collectNew []           = ([],[])
collectNew ((PatDiff.New s) : is) =
  let p = collectNew is
  in  (s : (fst p), snd $ p)
collectNew is            = ([],is)

collectBoth :: [PatDiff.Item Segment] -> (VString,[PatDiff.Item Segment])
collectBoth []           = ([],[])
collectBoth ((PatDiff.Both s s') : is) =
  let p = collectBoth is
  in  (s : (fst p), snd $ p)
collectBoth is            = ([],is)

processDiff :: [D.Diff VString] -> [Action]
processDiff []                 = []
processDiff (D.Both vs _ : ds) = (Copy vs): processDiff ds
processDiff (D.First vs : D.Second vs' : ds) = Compare vs vs' : processDiff ds
processDiff (D.First vs : ds)  = Copy vs : processDiff ds
processDiff (D.Second vs : ds) = Copy vs : processDiff ds 



str :: String ->  Segment
str s = Str $ T.pack s

showMerge (v,d) = "(" ++ showVText v ++","++show d++")"

-- | Unifying VText
-- >>> showVText $ unifyVText ([Chc 1 (plain "a") (plain "x"), Chc 1 (plain "b") (plain "y")])
-- "1<ab,xy>"
-- >>> showVText $ unifyVText ([str "l", str "m",Chc 1 (plain "a") (plain "x"), Chc 1 (plain "b") (plain "y")])
-- "lm1<ab,xy>"
-- >>> showVText $ unifyVText ([str "l", str "m",Chc 1 (plain "a") (plain "x"), Chc 1 ([Chc 2 (plain "b") (plain "i")]) (plain "y")])
-- "lm1<a2<b,i>,xy>"
-- >>> showVText $ unifyVText ([str "l", str "m",Chc 1 (plain "a") (plain "x"), Chc 2 ([Chc 3 (plain "b") (plain "i")]) (plain "y")])
-- "lm1<a,x>2<3<b,i>,y>"
-- >>> showVText $ unifyVText ([str "l",Chc 1 (plain "j") (plain "k"), str "m",Chc 1 (plain "a") (plain "x"), Chc 1 ([Chc 2 (plain "b") (plain "i")]) (plain "y")])
-- "l1<j,k>m1<a2<b,i>,xy>"
-- >>> showVText $ unifyVText ([Chc 1 ([str "a",str "b", Chc 2 (plain "c") (plain "d"), Chc 2 (plain "e") (plain "f")]) (plain "wxyz")])
-- "1<ab2<ce,df>,wxyz>"
-- >>> showVText $ unifyVText ([str "a", str " ", Chc 2 [Chc 1 (plain "") (plain "x"), Chc 1 (plain "") (plain " ")] [str "y "], str "b"])
-- "a 2<1<,x >,y >b"

-- | Renaming dimensions
-- >>> showVText $ renameDimensions 2 0 ([Chc 1 (plain "ab") ([Chc 2 (plain "x") (plain "z"),str "y"]),str "c"])
-- "3<ab,4<x,z>y>c"
-- >>> showVText $ renameDimensions 2 1 ( [Chc 1 (plain "ab") ([Chc 2 (plain "x") (plain "z"),str "y"]),str "c"])
-- "1<ab,3<x,z>y>c"
-- >>> showVText $ renameDimensions 2 2 ([Chc 1 (plain "ab") ([Chc 2 (plain "x") (plain "z"),str "y"]),str "c"])
-- "1<ab,2<x,z>y>c"


-- | merge 2 different vTexts.
--
-- Changes only in the left branch
-- >>> showMerge $ mergeCCNew 0 ( [str "ab ", Chc 1 ([str "c"]) ([str "x"])],1) ( [str "ab c"],0)
-- "(ab 1<c,x>,1)"
-- >>> showMerge $ mergeCCNew 0 ( [str "a ", Chc 1 ([str "b"]) ([str "x"]), str " c" ],1) ( [str "a b c"],0)
-- "(a 1<b,x> c,1)"
-- >>> showMerge $ mergeCCNew 0 ( [Chc 1 ([str "a"]) ([str "x"]), str " bc"],1) ( [str "a bc"],0)
-- "(1<a,x> bc,1)"
-- >>> showMerge $ mergeCCNew 0 ( [Chc 1 ([str "a"]) ([str "xy lm"]), str " bc"],1) ( [str "a bc"],0)
-- "(1<a,xy lm> bc,1)"
-- >>> showMerge $ mergeCCNew 0 ( [Chc 1 ([str "a"]) ([str ""]), str " bc"],1) ( [str "a bc"],0)
-- "(1<a,> bc,1)"
-- >>> showMerge $ mergeCCNew 0 ( [str "a ", Chc 1 ([str "bc"]) ([str "b"])],1) ( [str "a bc"],0)
-- "(a 1<bc,b>,1)"
-- >>> showMerge $ mergeCCNew 0 ( [str "a ", Chc 1 ([str ""]) ([str "y "]),str "b"],1) ( [str "a b"],0)
-- "(a 1<,y >b,1)"
--
-- | Changes only in the right branch
--
-- >>> showMerge $ mergeCCNew 0 ( [str "ab c"],0) ( [str "ab ", Chc 1 ([str "c"]) ([str "x"])],1)
-- "(ab 1<c,x>,1)"
-- >>> showMerge $ mergeCCNew 0 ( [str "a b c"],0) ( [str "a ", Chc 1 ([str "b"]) ([str "x"]), str " c" ],1)
-- "(a 1<b,x> c,1)"
-- >>> showMerge $ mergeCCNew 0 ( [str "a bc"],0) ( [Chc 1 ([str "a"]) ([str "x"]), str " bc"],1)
-- "(1<a,x> bc,1)"
-- >>> showMerge $ mergeCCNew 0 ( [str "a bc"],0) ( [Chc 1 ([str "a"]) ([str "xy lm"]), str " bc"],1)
-- "(1<a,xy lm> bc,1)"
-- >>> showMerge $ mergeCCNew 0 ( [str "a bc"],0) ( [Chc 1 ([str "a"]) ([str ""]), str " bc"],1)
-- "(1<a,> bc,1)"
-- >>> showMerge $ mergeCCNew 0 ( [str "a bc"],0) ( [str "a ", Chc 1 ([str "bc"]) ([str "b"])],1)
-- "(a 1<bc,b>,1)"
--
--
-- | Changes in both the branches
--
-- >>> showMerge $ mergeCCNew 0 ( [str "a", Chc 1 ([str "b"]) ([str "y"])],1) ( [str "a", Chc 2 ([str "b"]) ([str "x"])],2)
-- "(a2<1<b,y>,x>,2)"
-- >>> showMerge $ mergeCCNew 0 ( [Chc 2 ([str "a"]) ([str "x"]), str "b" ],2) ( [str "a", Chc 1 ([str "b"]) ([str "x"])],1)
-- "(2<a,x>1<b,x>,2)"
-- >>> showMerge $ mergeCCNew 0 ( [str "a ", Chc 2 ([str ""]) ([str "y "]),str "b"],2) ( [str "a ",Chc 1 ([str "b"]) ([str "x"])],1)
-- "(a 2<,y >1<b,x>,2)"
-- >>> showMerge $ mergeCCNew 0 ( [str "a ", Chc 1 ([str ""]) ([str "y "]),str "b"],1) ( [str "a ",Chc 2 ([str "b"]) ([str "x"])],2)
-- "(a 1<,y >2<b,x>,2)"
-- >>> showMerge $ mergeCCNew 0 ( [str "a ",Chc 1 ([str "b"]) ([str "x"])],1) ( [str "a ", Chc 2 ([str ""]) ( [str "y "]),str "b"],2)
-- "(a 2<,y >1<b,x>,2)"
-- >>> showMerge $ mergeCCNew 0 ( [str "a ", Chc 1 ( [str ""]) ( [str "x "]),str "b"],1) ( [str "a ", Chc 2 ( [str ""]) ( [str "y "]),str "b"],2)
-- "(a 2<1<,x >,y >b,2)"
-- >>> showMerge $ mergeCCNew 0 ( [str "a ", Chc 1 ( [str ""]) ( [str "x "]),str "b"],1) ( [str "a ", Chc 2 ( [str "b"]) ( [str ""])],2)
-- "(a 1<,x >2<b,>,2)"
--
-- | overlapping changes
--
-- >>> showMerge $ mergeCCNew 0 ( [Chc 1 ([str "a b"]) ([str "xy"]), str " c de"],1 ) ( [Chc 2 ([str "a"]) ([str "m"]), str " b c de"],2)
-- "(2<1<a,xy>,m>1< b,> c de,2)"
-- >>> showMerge $ mergeCCNew 0 ( [Chc 2 ([str "a"]) ([str "m"]), str " b c de"],2) ( [Chc 1 ([str "a b"]) ([str "xy"]), str " c de"],1 )
-- "(1<2<a,m> b,xy> c de,2)"
--
-- | Chain edits
-- >>> showMerge $ mergeCCNew 0 ( [Chc 1 (plain "a b") ([Chc 2 (plain "x") (plain "z"),str " y"]),str " c"],2) (plain "a b c",0)
-- "(1<a b,2<x,z> y> c,2)"
-- >>> showMerge $ mergeCCNew 0 ( [Chc 1 (plain "a b") ([Chc 2 (plain "x") (plain "z"),str "\ny"]),str " c"],2) ( [str "a ",Chc 3 (plain "b c") (plain "lm")],3)
-- "(1<a ,2<x,z>\n>3<1<b,y> c,lm>,3)"
--
-- | branch edits
-- >>> showMerge $ mergeCCNew 0 ( [Chc 1 ([Chc 2 (plain "x") (plain "z"),str " y"]) (plain "ab"),str " c"],2) (plain "x y c",0)
-- "(1<2<x,z> y,ab> c,2)"
-- >>> showMerge $ mergeCCNew 0 ( [Chc 1 ([Chc 2 (plain "x") (plain "z"),str " y"]) (plain "ab") ,str " c"],2) ( [str "x ",Chc 3 (plain "y c") (plain "lm")],3)
-- "(1<2<x,z> ,ab>3<1<y,> c,lm>,3)"
-- >>> showMerge $ mergeCCNew 0 ([Chc 1 (plain "x") (plain "y")],1) ([Chc 2 (plain "x") (plain "z")],2)
-- "(2<1<x,y>,z>,2)"

-- | branch and chain edits
-- >>> showMerge $ mergeCCNew 0 ( [Chc 1 ([Chc 2 (plain "x") (plain "z"),str " y"]) ([Chc 3 (plain "a") (plain "l"),str "\nb"]),str " c"],3) (plain "x y c",0)
-- "(1<2<x,z> y,3<a,l>\nb> c,3)"
--
-- >>> showMerge $ mergeCCNew 0 ( [Chc 1 ([Chc 2 (plain "x") (plain "z"),str " y"]) ([Chc 3 (plain "ab") (plain "l")]),str " c"],3) (plain "x y c",0)
-- "(1<2<x,z> y,3<ab,l>> c,3)"
--
-- >>> showMerge $ mergeCCNew 0 ( [Chc 1 ([Chc 2 (plain "x") (plain "z"),str " y"]) ([Chc 3 (plain "ab") (plain "l"),str "\nd"]),str " c"],3) (plain "x y c",0)
-- "(1<2<x,z> y,3<ab,l>\nd> c,3)"
--
-- >>> showMerge $ mergeCCNew 0 ( [Chc 1 ([Chc 2 (plain "x") (plain "z"),str " y"]) ([Chc 3 (plain "a") ( [Chc 4 (plain "l") (plain "s")]),str "\nb"]),str " c"],4) (plain "x y c",0)
-- "(1<2<x,z> y,3<a,4<l,s>>\nb> c,4)"
--
-- >>> showMerge $ mergeCCNew 0 ( [Chc 1 ([Chc 2 (plain "x") (plain "z"),str " y"]) (plain "ab") ,str " c"],2) ( [str "x ",Chc 3 (plain "y c") ( [Chc 4 (plain "l") (plain "s"),str " m"])],4)
-- "(1<2<x,z> ,ab>3<1<y,> c,4<l,s> m>,4)"
--
-- >>> showMerge $ mergeCCNew 0 ( [Chc 1 ([Chc 2 (plain "x") (plain "z"),str " y"]) ([Chc 3 (plain "a") (plain "l"),str "\nb"]),str " c"],3) ( [str "x ",Chc 4 (plain "y c") (plain "l")],4)
-- "(1<2<x,z> ,3<a,l>\n>4<1<y,b> c,l>,4)"
--
-- | Changes in both the branches. There can only be changes in one branch --**Update: Can there can be inner branches and chain edits in both the outer branches? **update: No
-- >> showMerge $ mergeCCNew 1 () ()
-- >>> showMerge $ mergeCCNew 0 ([Chc 3 (plain "x") (plain "z")], 3) ([Chc 1 [Chc 2  (plain "x") (plain "b")] (plain "a")],2)
-- "(1<2<3<x,z>,b>,a>,3)"
-- >>> showMerge $ mergeCCNew 0 ([Chc 2 (plain "x") [Chc 3 (plain "z") (plain "i")], str " y c"],3) ([Chc 1 (plain "x y ") [Chc 4 (plain "a") (plain "j"), str "\nb\n"], Chc 5 (plain "c") (plain "m")],5)
-- "(1<2<x,3<z,i>> y ,4<a,j>\nb\n>5<c,m>,5)"
-- >>> showMerge $ mergeCCNew 2 ( [Chc 1 ([Chc 2 (plain "x") (plain "z"),str " y"]) [Chc 3 (plain "a") (plain "i"), str "\nb"] ,str " c"],2) ([Chc 1 [Chc 2 [str "x"] [str "z"],str " y "] [Chc 4 [str "a"] [str "j"],str "\nb\n"],Chc 5 [str "c"] [str "m"]],5)
-- "(1<2<x,z> y ,4<3<a,i>,j>\nb\n>5<c,m>,5)"
--
-- >>> showMerge $ mergeCCNew 0 ( [Chc 1 (plain "a b") ([Chc 2 (plain "x") (plain "z"),str "\ny"]),str " c"],2) ( [Chc 3 (plain "a b c") ( [Chc 4 (plain "l\nm") ( [Chc 5 (plain "k") (plain "p")]),str "\no"])],5)
-- "(3<1<a b,2<x,z>\ny> c,4<l\nm,5<k,p>>\no>,5)"
--
--
-- | Branch after some modifications - i,e., there are some common dimesnions in both the VTexts
--
-- >>> showMerge $ mergeCCNew 1 ( [Chc 1 ([str "ab"]) ([str "x y"]), str "cde"],1 ) ( [Chc 1 ([str "ab"]) ([Chc 2 (plain "x") (plain "i"),str " y"]), str "cde"],2)
-- "(1<ab,2<x,i> y>cde,2)"
-- >>> showMerge $ mergeCCNew 1 ( [Chc 1 ([str "ab"]) ([Chc 2 (plain "x") (plain "i"),str " y"]), str "cde"],2 ) ( [Chc 1 ([str "ab"]) ([str "x y"]), str "cde"],1)
-- "(1<ab,2<x,i> y>cde,2)"
--
-- | Chain edits
-- >>> showMerge $ mergeCCNew 2 ( [Chc 1 (plain "a b") ([Chc 2 (plain "x") (plain "z"),str "\ny"]),str " c"],2) ( [Chc 1 (plain "a b") ([Chc 2 (plain "x") (plain "z"),str "\n",Chc 3 (plain "y") (plain "i")]),str " c"],3)
-- "(1<a b,2<x,z>\n3<y,i>> c,3)"
-- >>> showMerge $ mergeCCNew 2 ( [Chc 1 (plain "ab") ([Chc 2 (plain "x") (plain "z"),Chc 3 (plain "y") (plain "i")]),str "c"],3) ( [Chc 1 (plain "ab") ([Chc 2 (plain "x") (plain "z"),str "y"]),str "c"],2)
-- "(1<ab,2<x,z>3<y,i>>c,3)"
--
-- | branch edits
-- >>> showMerge $ mergeCCNew 2 ( [Chc 1 ([Chc 2 ([Chc 3 (plain "x") (plain "i") ]) (plain "z"),str "y"]) (plain "ab"),str "c"],3) ( [Chc 1 ([Chc 2 (plain "x") (plain "z"),str "y"]) (plain "ab"),str "c"],2)
-- "(1<2<3<x,i>,z>y,ab>c,3)"
--
-- | **update invalid editing scenario
-- >> showMerge $ mergeCCNew 2 ( [Chc 1 ([Chc 2 ([Chc 3 (plain "x") (plain "i") ]) (plain "z"),str "y"]) ([Chc 4 (plain "a") ([Chc 5 (plain "j") (plain "k")]),str "b"]),str "c"],3) ( [Chc 1 ([Chc 2 (plain "x") (plain "z"),str "y"]) ([Chc 4 (plain "a") (plain "j"),str "b"]),str "c"],2)
-- (*** Exception: Prelude.undefined
-- >>>showMerge $ mergeCCNew 0 ([Chc 4 [Chc 5 (plain "x") (plain "i")] (plain "z"), str " y c"],3) ([Chc 1 [Chc 2 (plain "x") (plain "z"), str " y"] [Chc 3 (plain "a") (plain "j"), str "\nb"], str " c"],4)
-- "(1<2<4<5<x,i>,z>,z> y,3<a,j>\nb> c,4)"
--
-- | conflicting merges
-- >>> showVText $ snd $ mergeVS ([RSel 1],[Chc 1 [str ""] [str "a ",Chc 2 [str ""] [str "b ", Chc 13 [str "\n\n\n"] [str "\n\nc x\n"], str "d "], Chc 4 [str "e"] [str "f"] ]]) ([RSel 2],[Chc 1 [str ""] [str "a ",Chc 2 [str ""] [str "b \n\n\nd "], Chc 4 [str "e"] [str "f"] ]])
-- "1<,a 2<,b 13<\n\n\n,\n\nc x\n>d >4<e,f>>"
-- >>> showVText $ snd $ mergeVS ([],[Chc 1 [str ""] [str "hi "]]) ([], [Chc 1 [str ""] [Chc 2[str ""][str "he ll o"], str "hi "]])
-- "1<,2<,he ll o>hi >"
--
-- >>> showVText $ snd $ mergeVS ([],([Chc 1 ([str "x\n",Chc 3 (plain "") (plain"b "), str "y"]) [str "l m"]])) ([],([Chc 1 ([str "x\n",Chc 3 (plain "") (plain"b "), str "y"]) [Chc 2 [str ""] [str "a "] ,str "l m"]]))
--

