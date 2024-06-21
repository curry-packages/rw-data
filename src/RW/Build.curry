--- Some conversion, selection and building goodies
---
--- @author Lasse Züngel

module RW.Build where

import AbstractCurry.Build
import AbstractCurry.Types as ACT
import AbstractCurry.Select
import FlatCurry.Types as FCT

import Data.List
import Data.Function
import Data.Maybe

import Debug.Trace

import qualified Data.Set as Set

--- Generates a list [0,1,2,...] with the same length as the input list
fromIndex0 :: [a] -> [Int]
fromIndex0 = fromIndex 0

--- Generates a list [i,i+1,i+2,...] with the same length as the input list
fromIndex :: Int -> [a] -> [Int]
fromIndex i xs = take (length xs) [i ..]

--- Generates a var name for a given index
varName :: Int -> String
varName j | j < 26    = [['a'..]!!j]                                   -- a,...,z
          | otherwise = [['a'..]!!(j `mod` 26)] ++ (show $ j `div` 26) -- b1,...,z1,b2,...,z2,...

--- Appends an element to a list if a given condition is true
appendIf :: Bool -> [a] -> a -> [a]
appendIf True  xs x = xs ++ [x]
appendIf False xs _ = xs

--- Removes the first element from a list if the first list's length is 1. Otherwise, the list is returned unchanged.
optimizeSingleConstructor :: [a] -> [b] -> [b]
optimizeSingleConstructor xs ys = case xs of
  [_] -> drop 1 ys
  _   -> ys

---------------------------- Type Declarations and Expression  ------------------------------------

--- Returns the amount of arguments of a given type constructor
consArity :: CConsDecl -> Int
consArity (CCons   _ _ tes) = length tes
consArity (CRecord _ _ fds) = length fds

--- Returns the arguments (type expressions) of a given type constructor
consTypeExpressions :: CConsDecl -> [CTypeExpr]
consTypeExpressions (CCons   _ _ tes) = tes
consTypeExpressions (CRecord _ _ fds) = map getTE fds
  where
    getTE (CField _ _ te) = te

--- Returns true if the given type declaration is polymorphic (contains at least one type variable)
isPolymorphic :: CTypeDecl -> Bool
isPolymorphic (CType _ _ tvs _ _) = not $ null tvs

--- Returns true if the given type declaration is monomorphic (contains no type variables)
isMonomorphic = not . isPolymorphic

--- All type variables occurring in a given type declaration
typeVars :: CTypeDecl -> [CTVarIName]
typeVars (CType _ _ tvs _ _)    = tvs
typeVars (CTypeSyn _ _ tvs _)   = tvs
typeVars (CNewType _ _ tvs _ _) = tvs

--- Converts a recursive type expression to a sequence of type expressions
---
--- Example: 
---  unwrapApply (CTApply (CTApply (Cons "map") (Var "x")) (Var "y")) = [Cons "map", Var "x", Var "y"]
unwrapApply :: CTypeExpr -> [CTypeExpr]
unwrapApply te = case te of
                  CTApply a b -> unwrapApply a ++ [b]
                  _           -> [te]

--- All type variables occurring in a given constructor declaration
consVars :: CConsDecl -> [CTVarIName]
consVars (CCons _ _ tes) = [expr2name x | x <- tes, isTypeVar x]
  where
    expr2name e = case e of
                    (CTVar n) -> n
                    _         -> error "consVars: not a type variable"

--- Returns true if the given type expression is a type variable 
isTypeVar :: CTypeExpr -> Bool
isTypeVar te = case te of
                 CTVar _ -> True
                 _       -> False

--- The type variable used as the placeholder for instance declarations of the ReadWrite class 
genericTypeVariable :: CTypeExpr
genericTypeVariable = CTVar (0, "a")

--- Generates the rwClass constraint for all type variables.
--- Given a type
---   data T t1 ... tn = ...
--- This function can be used to derive all constraints necessary to generate the instance header
---   instance (ReadWrite t1, ..., ReadWrite tn) => ReadWrite (T t1 ... tn) 
classConstraint :: String -> String -> [CTVarIName] -> [CConstraint]
classConstraint className module' = map (\n -> ((module', className), CTVar n))

--- May or may not add the input type to the output. For a given call
---  > returnTypeExpr r a b
--- If r is true, then the resulting type expression is 
---   fun :: b -> (a, b)
--- Otherwise, the resulting expression is
---   fun :: b -> a
---
--- This is useful when building type expressions for the read function.
returnTypeExpr :: Bool -> CTypeExpr -> CTypeExpr -> CTypeExpr
returnTypeExpr True  te format = format ~> (CTApply (CTApply (CTCons ("Prelude", "(,)")) te) format)
returnTypeExpr False _  format = format ~> CTVar (0, "a")

--- Combines all given expressions with a given operator (right-associative)
combineWithR :: FCT.QName -> [CExpr] -> CExpr
combineWithR op = foldr1 (\x y -> applyF op [x,y])

--- Combines all given expressions with a given operator (left-associative)
combineWithL :: FCT.QName -> [CExpr] -> CExpr
combineWithL op = foldl1 (\x y -> applyF op [x,y])

--- Concats all given expressions with the ++ operator
concatExpr :: [CExpr] -> CExpr
concatExpr = combineWithR (pre "++")

--- Combines all given expressions with the . operator
applyExpr :: [CExpr] -> CExpr
applyExpr = combineWithR (pre ".")

--- Builds an expression 'e1 == e2'
equalsExpr :: CExpr -> CExpr -> CExpr
equalsExpr e1 e2 = applyF (pre "==") [e1, e2]

--- 'otherwise', used for guards
otherwiseExpr :: CExpr
otherwiseExpr = CSymbol $ pre "otherwise"

returnExpr :: CExpr -> CStatement
returnExpr e = CSExpr (CApply (CSymbol (pre "return")) e)

--- Converts a type decl
---   data T t1 ... tn = ...
--- to a type expression
---   T1 t1 ... tn
typeDeclToTypeExpr :: CTypeDecl -> CTypeExpr
typeDeclToTypeExpr decl = case decl of
  (CType name _ []  _ _) -> CTCons name
  (CType name _ tvs _ _) -> let type' = CTCons name : map CTVar tvs
                            in foldl1 CTApply type'

--- Converts a type decl
---   data T t1 ... tn = ...
--- to a string
---   T
typeDeclToName :: CTypeDecl -> String
typeDeclToName decl = case decl of
  (CType (_, name) _ _ _ _) -> name

--- Returns true iff the given type declaration is a type synonym
isTypeSyn :: CTypeDecl -> Bool
isTypeSyn t = case t of
  (CTypeSyn _ _ _ _) -> True
  _                  -> False

--- Simple pretty printer for type expressions
showTypeExpr (CTApply a b)      = showTypeExpr a ++ " " ++ showTypeExpr b
showTypeExpr (CTCons (_, name)) = name
showTypeExpr (CTVar (_, name))  = name
showTypeExpr (CFuncType a b)    = "(" ++ showTypeExpr a ++ " -> " ++ showTypeExpr b ++ ")"

--- Looks up an existing(!) constructor based on its name
theCons :: ACT.QName -> CTypeDecl -> CConsDecl
theCons cn  = fromJust . find ((== cn) . consName) . typeCons 

-------------------------------------- Patterns ---------------------------------------------------

--- Anonymous pattern "_"
anonPattern :: CPattern
anonPattern = CPVar (0, "_")

--- Constructs a head:rest-pattern from a list of patterns. For n passed patterns, the resulting pattern is
---   p1:p2:...:pn
--- If the list is empty, the resulting pattern is
---   []
listRestPattern :: [CPattern] -> CPattern
listRestPattern xs = case xs of
                      [] -> pNil
                      _  -> foldr1 (\x y -> CPComb (pre ":") [x,y]) xs

--- Pattern-matches the first occurrence of every type variable in a given type expression. 
---
--- For a specific constructor of a type definition 
---   data T t1 ... tn = ... | C e1 ... em | ...
--- this function generates a pattern
---  C p1 ... pm
--- where pi (1 <= i <= m) is...
---  - a variable ei' if ei the first occurence of a polymorphic type variable t1 ... tn in the constructor
---  - a wildcard _   otherwise
---
--- Example:
---   data T a = C a String a [a]
---   consToPolyPattern C -> C a' _ _ _
consToPolyPattern :: CConsDecl -> CPattern
consToPolyPattern (CCons name _ tes) = CPComb name pats
  where
    pats = map convert tes
    -- Converts a type expression to a pattern such that only type variables are pattern-matched (bound to a variable)
    convert te = case te of
      CTVar (i, n) -> CPVar (i, n ++ "'")
      _            -> anonPattern
    -- Replaces all recurrences of type variables in a list of type expressions with wildcards
    anonDuplicates (x:xs) | x == anonPattern = x : anonDuplicates xs
                          | otherwise        = x : anonDuplicates (substitute x anonPattern xs)
    anonDuplicates []     = []
    -- Replaces all occurrences of a type variable pattern with a wildcard pattern
    substitute v1 v2 = map (\x -> if x == v1 then v2 else x) 

--- Generates a useless rule. 
undefinedConstructorRule :: CConsDecl -> CRule
undefinedConstructorRule cons = case cons of
    (CCons name _ tes) -> CRule [CPComb name (map (\i -> CPVar (i, varName i)) (fromIndex0 tes))] (CSimpleRhs (CSymbol ("Huh?", "undefined")) [])
    _                  -> CRule [] (CSimpleRhs (CSymbol ("Huh?", "fail")) [])

-------------------------------------- CurryProg --------------------------------------------------

--- Returns the instance declarations of a curry program
instances :: ACT.CurryProg -> [ACT.CInstanceDecl]
instances (CurryProg _ _ _ _ is _ _ _) = is

--- Returns the name of an instance declaration
instanceName :: ACT.CInstanceDecl -> ACT.QName
instanceName (CInstance name _ _ _) = name

--- Converts a flatcurry program to an abstractcurry program.
--- Extracts only the name, imports and type definitions.
---
--- Converts FCY type synonyms to ACT data declarations.
---
--- Naming scheme for type variables of polymorphic type definitions:
---   data T a ... z a1 ... z1 ... = ...
flatProgToAbstract :: FCT.Prog -> ACT.CurryProg
flatProgToAbstract (FCT.Prog name is ts _ _) = ACT.CurryProg name is Nothing [] [] (filter (not . isPrefixOf "_Dict#" . typeDeclToName) (map flatTypeDeclToAbstract ts)) [] []
  where
    flatTypeDeclToAbstract t = case t of
      (FCT.Type qn vis tvar cds)  -> ACT.CType qn (flatVisiblityToAbstract vis) (map flatTypeVarToAbstract tvar) (map flatConstrDeclToAbstract cds) []
      (FCT.TypeNew qn vis tvar c) -> ACT.CType qn (flatVisiblityToAbstract vis) (map flatTypeVarToAbstract tvar) [flatNewConsToAbstractCons c] []
    flatVisiblityToAbstract v = case v of
      FCT.Public  -> ACT.Public
      FCT.Private -> ACT.Private
    flatTypeVarToAbstract (index, _) = (index, varName index)
    flatConstrDeclToAbstract (FCT.Cons qn _ vis tes) = ACT.CCons qn (flatVisiblityToAbstract vis) (map flatTypeExprToAbstract tes)
    flatNewConsToAbstractCons (FCT.NewCons qn vis te) = ACT.CCons qn (flatVisiblityToAbstract vis) [flatTypeExprToAbstract te]
    flatTypeExprToAbstract fte = case fte of
      FCT.TVar index     -> ACT.CTVar (index, varName index)
      FCT.FuncType t1 t2 -> ACT.CFuncType (flatTypeExprToAbstract t1) (flatTypeExprToAbstract t2)
      FCT.TCons qn tes   -> case tes of
        [] -> ACT.CTCons qn
        _  -> applyTC qn (map flatTypeExprToAbstract tes)
      _ -> error "flatTypeExprToAbstract: forall quantifier not supported yet"

--- auxiliary util functions

--- If just a value is given, the predicate is applied to the value. Otherwise, false is returned.
whenJust :: Maybe a -> (a -> Bool) -> Bool
whenJust (Just x) f = f x
whenJust Nothing  _ = False

--- not . any
none :: (a -> Bool) -> [a] -> Bool
none f = not . any f

--- snd of a triple
snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x