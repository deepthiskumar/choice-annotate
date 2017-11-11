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

type DimVText = (VString,Dim)
type DimSegment = (Segment,Dim)


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


--split the plain string into characters
granulatePlain :: [Segment] -> [Segment]
granulatePlain [] = []
granulatePlain ((Str a) : xs)   = (getPlainChar a) ++ granulatePlain xs
granulatePlain ((Chc d l r : xs)) = (Chc d (granulatePlain l) (granulatePlain r)) : granulatePlain xs

{-getPlainChar :: [Char] -> [Segment]
getPlainChar []     = [Str ""]
getPlainChar [x]    = [Str [x]]
getPlainChar (x:xs) = Str [x] : getPlainChar xs-}

getPlainChar :: Text -> [Segment]
getPlainChar s 
  | T.length s == 0 = [Str T.empty]
  | T.length s == 1 = [Str s]
  | otherwise       = (Str $ T.singleton $ T.head s) : (getPlainChar $ T.tail s)

-- |
-- >>> showVText $ vSplit ( [str "abc"])
-- "abc"
--
-- >>> showVText $ vSplit ( [str "ab", Chc 1 ([str "c"]) ([str "x"])])
-- "ab1<c,x>"
--
-- >>> showVText $ vSplit ( [str "a", Chc 1 ([str "b"]) ([str "y"])])
-- "a1<b,y>"
--
-- >>> showVText $ vSplit ( [Chc 1 (plain "ab") ([Chc 2 (plain "xy") (plain "zl"),str "ymn"]),str "crty"])
-- "1<a,2<x,z>>1<b,2<y,l>>1<,y>1<,m>1<,n>crty"
--
-- >>> showVText $ vSplit ( [Chc 1 ([Chc 2 (plain "xy") (plain "zl"),str "ymn"]) (plain "ab"),str "crty"])
-- "1<2<x,z>,a>1<2<y,l>,b>1<y,>1<m,>1<n,>crty"
--

vSplit::VString -> VString
vSplit x = agressiveSplit (granulatePlain x)

--Splits the choice expression at character level
agressiveSplit :: VString -> VString
agressiveSplit []     = []
agressiveSplit (x:xs) = let ys = splitSegment x
                            zs = agressiveSplit xs
                        in (ys ++ zs)

splitSegment :: Segment -> [Segment]
splitSegment (Str x) = [Str x]
splitSegment (Chc d l r) = case (l,r) of
                            ([],[])                     -> []
                            ([],ys)                     -> splitSegment (Chc d (plain "") ys )
                            (xs,[])                     -> splitSegment (Chc d xs (plain ""))
                            (Str x : xs,Str y : ys) -> (Chc d [Str x] [Str y]) : (splitSegment (Chc d xs ys))
                            (x : xs, y : ys)            -> let (x':xs') =  splitSegment x
                                                               (y':ys') =  splitSegment y
                                                           in (Chc d [x'] [y']) : splitSegment (Chc d (xs'++xs) (ys'++ys))

unifyVText :: VString -> VString
unifyVText []       = []
unifyVText [s]      = case s of
                        Str _   -> [s]
                        Chc d l r -> [Chc d (unifyVText l) (unifyVText r)]
unifyVText (s:t:ss) = case unifySegment s t of
                                  Just u  -> unifyVText (u:ss)
                                  Nothing -> let us = unifyVText (t:ss)
                                             in (s:us)

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
mergeVText (s:ss) (d:ds) = mergeChoices s d ss ds
                                                                                        
mergeChoices :: Segment -> Segment -> VString -> VString -> VString
mergeChoices s@(Chc x [Str s'] r) d@(Chc y [Str d'] r') ss ds
  | T.length s' == 0 && T.length d' == 0 = let d' = mergeSegment s d
                                               ds' = mergeVText ss ds
                                           in (d' : ds')
mergeChoices c@(Chc x [Str s'] r) d ss ds 
  | T.length s' == 0   = let ds' = mergeVText ss (d:ds)
                         in (c : ds')
mergeChoices s c@(Chc y [Str s'] r) ss ds 
  | T.length s' == 0   = let ds' = mergeVText (s:ss) ds
                         in (c : ds')
mergeChoices s d ss ds = let d' = mergeSegment s d
                             ds' = mergeVText ss ds
                         in (d' : ds')                                                        

mergeSegment :: Segment -> Segment -> Segment
mergeSegment (Str a) (Str b)        = (Str b) --Invariant 1 If a character in the same position is different in both the files, then atleast either one has to be a choice
mergeSegment (Chc d l r) (Str b)      = (Chc d l r)
mergeSegment (Str a) (Chc d l r)      = (Chc d l r)
mergeSegment c1@(Chc d l r) c2@(Chc d' l' r')
   | c1 == c2 = c2
   | otherwise = mergeChoice c1 c2

mergeChoice :: Segment -> Segment -> Segment
mergeChoice c1@(Chc d l r) c2@(Chc d' l' r')
  | d == d'   = case l == l' of --left or right alternative has changes
                 True  -> case r == r' of
                           True  -> c2
                           False -> Chc d l' (mergeVText r r')
                 False -> case r == r' of
                           True  -> Chc d (mergeVText l l') r'
                           False -> undefined --Chc d (mergeVText l l') (mergeVText r r') --Invariant 2. This is not allowed (an invariant we impose). A choice can have edits either in left or right alternatives. Not both
  | otherwise = (Chc d' [(Chc d l r)] r')-- these are new choices introduced and put c1 in the left branch of c2

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
                                
                                --TODO
                                --CHNGE SPLIT TO TOKENIZE
                                -- AND THEN GENERALIZE THIS

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
-- >>> showMerge $ mergeCC 0 ( [str "ab", Chc 1 ([str "c"]) ([str "x"])],1) ( [str "abc"],0)
-- "(ab1<c,x>,1)"
-- >>> showMerge $ mergeCC 0 ( [str "a", Chc 1 ([str "b"]) ([str "x"]), str "c" ],1) ( [str "abc"],0)
-- "(a1<b,x>c,1)"
-- >>> showMerge $ mergeCC 0 ( [Chc 1 ([str "a"]) ([str "x"]), str "bc"],1) ( [str "abc"],0)
-- "(1<a,x>bc,1)"
--
-- | Changes only in the right branch
--
-- >>> showMerge $ mergeCC 0 ( [str "abc"],0) ( [str "ab", Chc 1 ([str "c"]) ([str "x"])],1)
-- "(ab1<c,x>,1)"
-- >>> showMerge $ mergeCC 0 ( [str "abc"],0) ( [str "a", Chc 1 ([str "b"]) ([str "x"]), str "c" ],1)
-- "(a1<b,x>c,1)"
-- >>> showMerge $ mergeCC 0 ( [str "abc"],0) ( [Chc 1 ([str "a"]) ([str "x"]), str "bc"],1)
-- "(1<a,x>bc,1)"
--
-- | Changes in both the branches
--
-- >>> showMerge $ mergeCC 0 ( [str "a", Chc 1 ([str "b"]) ([str "y"])],1) ( [str "a", Chc 1 ([str "b"]) ([str "x"])],1)
-- "(a1<2<b,y>,x>,2)"
-- >>> showMerge $ mergeCC 0 ( [Chc 1 ([str "a"]) ([str "x"]), str "b" ],1) ( [str "a", Chc 1 ([str "b"]) ([str "x"])],1)
-- "(2<a,x>1<b,x>,2)"
-- >>> showMerge $ mergeCC 0 ( [str "a", Chc 1 ([str ""]) ([str "y"]),str "b"],1) ( [str "ab"],0)
-- "(a1<,y>b,1)"
--
-- >>> showMerge $ mergeCC 0 ( [str "a", Chc 1 ([str ""]) ([str "y"]),str "b"],1) ( [str "a",Chc 1 ([str "b"]) ([str "x"])],1)
-- "(a2<,y>1<b,x>,2)"
-- >>> showMerge $ mergeCC 0 ( [str "a",Chc 1 ([str "b"]) ([str "x"])],1) ( [str "a", Chc 1 ([str ""]) ( [str "y"]),str "b"],1)
-- "(a1<,y>2<b,x>,2)"
-- >>> showMerge $ mergeCC 0 ( [str "a", Chc 1 ( [str ""]) ( [str "x"]),str "b"],1) ( [str "a", Chc 1 ( [str ""]) ( [str "y"]),str "b"],1)
-- "(a1<2<,x>,y>b,2)"
--
-- | overlapping changes
--
-- >>> showMerge $ mergeCC 0 ( [Chc 1 ([str "ab"]) ([str "xy"]), str "cde"],1 ) ( [Chc 1 ([str "a"]) ([str "m"]), str "bcde"],1)
-- "(1<2<a,x>,m>2<b,y>cde,2)"
-- >>> showMerge $ mergeCC 0 ( [Chc 1 ([str "a"]) ([str "m"]), str "bcde"],1 ) ( [Chc 1 ([str "ab"]) ([str "xy"]), str "cde"],1)
-- "(1<2<a,m>b,xy>cde,2)"
--
-- | Chain edits
-- >>> showMerge $ mergeCC 0 ( [Chc 1 (plain "ab") ([Chc 2 (plain "x") (plain "z"),str "y"]),str "c"],2) (plain "abc",0)
-- "(1<ab,2<x,z>y>c,2)"
-- >>> showMerge $ mergeCC 0 ( [Chc 1 (plain "ab") ([Chc 2 (plain "x") (plain "z"),str "y"]),str "c"],2) ( [str "a",Chc 1 (plain "bc") (plain "lm")],1)
-- "(2<a,3<x,z>>1<2<b,y>c,lm>,3)"
--
-- | branch edits
-- >>> showMerge $ mergeCC 0 ( [Chc 1 ([Chc 2 (plain "x") (plain "z"),str "y"]) (plain "ab"),str "c"],2) (plain "xyc",0)
-- "(1<2<x,z>y,ab>c,2)"
-- >>> showMerge $ mergeCC 0 ( [Chc 1 ([Chc 2 (plain "x") (plain "z"),str "y"]) (plain "ab") ,str "c"],2) ( [str "x",Chc 1 (plain "yc") (plain "lm")],1)
-- "(2<3<x,z>,a>1<2<y,b>c,lm>,3)"
--

-- | branch and chain edits
-- >>> showMerge $ mergeCC 0 ( [Chc 1 ([Chc 2 (plain "x") (plain "z"),str "y"]) ([Chc 3 (plain "a") (plain "l"),str "b"]),str "c"],3) (plain "xyc",0)
-- "(1<2<x,z>y,3<a,l>b>c,3)"
--
-- >>> showMerge $ mergeCC 0 ( [Chc 1 ([Chc 2 (plain "x") (plain "z"),str "y"]) ([Chc 3 (plain "ab") (plain "l")]),str "c"],3) (plain "xyc",0)
-- "(1<2<x,z>y,3<ab,l>>c,3)"
--
-- >>> showMerge $ mergeCC 0 ( [Chc 1 ([Chc 2 (plain "x") (plain "z"),str "y"]) ([Chc 3 (plain "ab") (plain "l"),str "d"]),str "c"],3) (plain "xyc",0)
-- "(1<2<x,z>y,3<ab,l>d>c,3)"
--
-- >>> showMerge $ mergeCC 0 ( [Chc 1 ([Chc 2 (plain "x") (plain "z"),str "y"]) ([Chc 3 (plain "a") ( [Chc 4 (plain "l") (plain "s")]),str "b"]),str "c"],4) (plain "xyc",0)
-- "(1<2<x,z>y,3<a,4<l,s>>b>c,4)"
--
-- >>> showMerge $ mergeCC 0 ( [Chc 1 ([Chc 2 (plain "x") (plain "z"),str "y"]) (plain "ab") ,str "c"],2) ( [str "x",Chc 1 (plain "yc") ( [Chc 2 (plain "l") (plain "s")]),str "m"],2)
-- "(3<4<x,z>,a>1<3<y,b>c,2<l,s>>m,4)"
--
-- >>> showMerge $ mergeCC 0 ( [Chc 1 ([Chc 2 (plain "x") (plain "z"),str "y"]) ([Chc 3 (plain "a") (plain "l"),str "b"]),str "c"],3) ( [str "x",Chc 1 (plain "yc") (plain "l")],1)
-- "(2<3<x,z>,4<a,l>>1<2<y,b>c,l>,4)"
--
-- | Changes in both the branches. There can only be changes in one branch
-- >> showMerge $ mergeCC 2 ( [Chc 1 ([Chc 2 (plain "x") (plain "z"),str "y"]) (plain "ab") ,str "c"],2) ( [Chc 1 ([Chc 2 (plain "x") ( [Chc 3 (plain "z") (plain "i")]),str "y"]) ([Chc 3 (plain "a") (plain "j"),str "b"]),Chc 1 (plain "c") (plain "m")],3)
-- (*** Exception: Prelude.undefined
--
-- >>> showMerge $ mergeCC 0 ( [Chc 1 (plain "ab") ([Chc 2 (plain "x") (plain "z"),str "y"]),str "c"],2) ( [Chc 1 (plain "abc") ( [Chc 2 (plain "lm") ( [Chc 3 (plain "k") (plain "p")]),str "o"])],3)
-- "(1<4<ab,5<x,z>y>c,2<lm,3<k,p>>o>,5)"
--
--
-- | Branch after some modifications - i,e., there are some common dimesnions in both the VTexts
--
-- >>> showMerge $ mergeCC 1 ( [Chc 1 ([str "ab"]) ([str "xy"]), str "cde"],1 ) ( [Chc 1 ([str "ab"]) ([Chc 2 (plain "x") (plain "i"),str "y"]), str "cde"],2)
-- "(1<ab,2<x,i>y>cde,2)"
-- >>> showMerge $ mergeCC 1 ( [Chc 1 ([str "ab"]) ([Chc 2 (plain "x") (plain "i"),str "y"]), str "cde"],2 ) ( [Chc 1 ([str "ab"]) ([str "xy"]), str "cde"],1)
-- "(1<ab,2<x,i>y>cde,2)"
--
-- | Chain edits
-- >>> showMerge $ mergeCC 2 ( [Chc 1 (plain "ab") ([Chc 2 (plain "x") (plain "z"),str "y"]),str "c"],2) ( [Chc 1 (plain "ab") ([Chc 2 (plain "x") (plain "z"),Chc 3 (plain "y") (plain "i")]),str "c"],3)
-- "(1<ab,2<x,z>3<y,i>>c,3)"
-- >>> showMerge $ mergeCC 2 ( [Chc 1 (plain "ab") ([Chc 2 (plain "x") (plain "z"),Chc 3 (plain "y") (plain "i")]),str "c"],3) ( [Chc 1 (plain "ab") ([Chc 2 (plain "x") (plain "z"),str "y"]),str "c"],2)
-- "(1<ab,2<x,z>3<y,i>>c,3)"
--
-- | branch edits
-- >>> showMerge $ mergeCC 2 ( [Chc 1 ([Chc 2 ([Chc 3 (plain "x") (plain "i") ]) (plain "z"),str "y"]) (plain "ab"),str "c"],3) ( [Chc 1 ([Chc 2 (plain "x") (plain "z"),str "y"]) (plain "ab"),str "c"],2)
-- "(1<2<3<x,i>,z>y,ab>c,3)"
--
-- >> showMerge $ mergeCC 2 ( [Chc 1 ([Chc 2 ([Chc 3 (plain "x") (plain "i") ]) (plain "z"),str "y"]) ([Chc 4 (plain "a") ([Chc 5 (plain "j") (plain "k")]),str "b"]),str "c"],3) ( [Chc 1 ([Chc 2 (plain "x") (plain "z"),str "y"]) ([Chc 4 (plain "a") (plain "j"),str "b"]),str "c"],2)
-- (*** Exception: Prelude.undefined
--
