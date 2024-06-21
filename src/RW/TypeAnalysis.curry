--- DEPRECATED ---
--- Module for analyzing type declarations. Used by the 'typeOf' function generator.
---
--- @author Lasse Züngel

module RW.TypeAnalysis where

import AbstractCurry.Types
import AbstractCurry.Select
import AbstractCurry.Build

import Data.List 

import RW.Build

--- The position of a type variable in a constructor declaration
type VarPos = [Int]

rootPos :: VarPos
rootPos = []

--- Given some constructor Cn of type T
---   data T t1 ... tn = ... | Cn e1 ... em | ...
--- this type represents the assignment of constructor parameter positions 
--- to type variables. 
---
--- Examples: 
--- 
--- Consider 
---   data Either a b = Left a | Right b 
--- Then, the assignments for the constructors are
---   1. For "Left":  (0, [a, []])
---   2. For "Right": (0, [b, []])
--- 
--- Consider
---   data MyData a b = MyCons (Pair a b) 
--- Then, the assignment for the constructor is
---  1. For "MyCons": (0, [(a, [0]), (b, [1])])
data Assignment = Assignment QName [(Int, [(CTVarIName, VarPos)])]
  deriving (Show)

--- Given some type variable assignments, this function returns a collection of all type variables,
--- associated with their constructor and the position of the type variable in the constructor.
assignments2typeAccess :: [Assignment] -> [(CTVarIName, (VarPos, QName, Int))]
assignments2typeAccess = concatMap (\(Assignment cons poss) -> concatMap (\(i, poss') -> map (\(var, pos) -> (var, (pos, cons, i))) poss') poss)

--- For a given type declaration, this function generates a list of assignments.
--- The result is non-redundant and correct, i.e., all type variables are assigned 
--- exactly once. 
---
--- Consider 
---   data Foo a b c = C0 c | C1 (Maybe (Maybe a)) (Maybe b)
--- Then, the result contains 
---   - the constructor "C0" binding the type variable "c" to the position 0 ([])
---   - the constructor "C1" binding the type variable "a" to the position 0 ([0,0])
---                              and the type variable "b" to the position 1 ([0])
---   
--- The chosen type variable position can't be inside the type constructor of the same type. Consider
---   data MyLoop a = MyCons (MyLoop a) (Maybe (Maybe (Maybe a)))
---                                                         ^^^
--- Then, the type variable position is as marked.
---
--- Note that the varPos is stripped of the constructor parameter position.
--- 
--- Using the assignments, one can locate all type variables in the type declaration. 
completeAssignment :: [TypeAnalysis] -> CTypeDecl -> [Assignment]
completeAssignment tas ctd = foldr addAssignment [] res
  where
    res = minimumBy ordVars allVarOccs
    typename = typeName ctd

    allVars = allVarPos ctd
    -- All possible variable occurences in a given constructor minus the recursive ones 
    allVarOcc (var, occurence) = filter (\(var, _, (_, dep)) -> isCyclefree (var, dep) typename tas) 
                                  [(var, cn, pos) | (cn, poss) <- occurence, pos <- poss]

    -- All combinations that yield a full set of assignments 
    allVarOccs = sequence $ filter (not . null) $ map allVarOcc allVars

    ordVars xs ys = compare (evaluateAssignment xs) (evaluateAssignment ys)
    evaluateAssignment xs = sum (map evaluatePos xs) + (length (nub $ map snd3 xs) * 10 )
    evaluatePos (_, _, pos) = length $ fst pos

    addAssignment (var, cons, (i:pos, _)) asss = case asss of
      [] -> [Assignment cons [(i, [(var, pos)])]]
      _  -> case findAssignment cons asss of
        Just (Assignment cons' poss) -> Assignment cons' (addPoss i var pos poss) : filter (\(Assignment cons'' _) -> cons'' /= cons') asss
        Nothing                      -> Assignment cons [(i, [(var, pos)])] : asss
    findAssignment cons  = find (\(Assignment cons' _) -> cons' == cons)

    addPoss i var pos poss = case poss of
      [] -> [(i, [(var, pos)])]
      _  -> case findPoss i poss of
        Just (i', poss') -> (i', (var, pos) : poss') : filter (\(i'', _) -> i'' /= i') poss
        Nothing          -> (i, [(var, pos)]) : poss
    findPoss i = find (\(i', _) -> i' == i) 

--- Checks if a single variable occurrence (position) is cycle-free, i.e., if the variable is not bound to a type constructor that's 
--- dependent on the current type.
---
--- > isCyclefree (a, [(T2, 0)]) T1 Ts = True
--- if the 0th type variable of T2 is not bound to a type constructor of T1.
isCyclefree :: (CTVarIName, TypeDependencies) -> Typename -> [TypeAnalysis] -> Bool
isCyclefree (name, deps) typename = all cycleFreePerType       
  where
    -- No 1-cycle (self-reference) and no use of local data types (definitely not a cycle of any kind)
    noTypeDependence tn = tn /= typename && tn `notElem` (map fst deps)

    -- All types are cycle-free
    cycleFreePerType (tn, poss) | noTypeDependence tn = True
                                | otherwise           = all cycleFreePerVar poss    
      where
        -- Check all type variables of the other type
        cycleFreePerVar ((iOther, _), gdeps, occs) | iOther `notInDep` deps = True
                                                   | otherwise              = all cycleFreePerDep gdeps
          where
            cycleFreePerDep (tn', i) = tn' /= typename || i `notInDep` deps

            varIndex `inDep`    deps = any (\(tn', i) -> tn' == tn && i == iOther) deps
            varIndex `notInDep` deps = not $ varIndex `inDep` deps


--- Returns all occurrences (positions in constructor declarations) 
--- of all type variables in a given type declaration, given that 
--- the type variable's position is not inside a type constructor of the same type.
---
--- Consider
---   data MyData a = MyCons a (MyData a) (Maybe (Maybe (Maybe a))) | MyHuh a a a
---                          ^         x                       ^            ^ ^ ^
--- All marked (^) positions are part of the result while the position marked with (x) is not.
--- This is the case because we need to bind the type variable somehow in order to infer the 
--- type of the currently processed type declaration. If we used the same type to 
--- derive the type of the type variable, we would end up with an infinite loop.
allVarPos :: CTypeDecl -> [(CTVarIName, [VarOccurrence])]
allVarPos typedecl@(CType _ _ tvs cons _) =  map perTypeVar tvs
  where
    perTypeVar tv = (tv, map (\c -> (consName c, varPos c tv)) cons)

    -- Returns the positions of a type variable in a constructor declaration
    varPos :: CConsDecl -> CTVarIName -> [(VarPos, TypeDependencies)]
    varPos x = case x of
      (CCons _ _ tes)   -> varPos' tes
      (CRecord _ _ fds) -> varPos' (map getTE fds)
        where
          getTE (CField _ _ te) = te
      where
        -- Returns the positions of a type variable in a list of type expressions
        varPos' :: [CTypeExpr] -> CTVarIName -> [(VarPos, TypeDependencies)]
        varPos' tes n = concatMap (\(i, te) -> appendPos i $ varPos'' te) (zip [0..] tes)
          where
            varPos'' :: CTypeExpr -> [(VarPos, TypeDependencies)]
            varPos'' te = case te of
              CTVar n' | n == n' -> [(rootPos, [])] 
              app@(CTApply a b)  -> let (c:ts) = unwrapApply app
                                    in case c of
                                      CTCons cn | cn /= (typeName typedecl)
                                              -> concatMap (\(i, t) -> appendPos i $ addDependency (cn, i) $ varPos'' t) $ zip [0..] ts
                                      _       -> []
              _                  -> []

--- Returns, for all type declarations, all type variables and their occurrences.
moduleVarPos :: [CTypeDecl] -> [TypeAnalysis]
moduleVarPos ts = let start = map (\t -> (typeName t, map addGeneralDependencies (allVarPos t))) ts
                  in fixpoint start
  where
    -- Collects all direct dependencies of a type variable
    addGeneralDependencies :: (CTVarIName, [VarOccurrence]) -> (CTVarIName, TypeDependencies, [VarOccurrence])
    addGeneralDependencies (varName, occs) = (varName, gdep, occs) 
    where
      gdep :: TypeDependencies 
      gdep = nub $ concatMap (\(_, varPoss) -> concatMap snd varPoss) occs

    -- Computes the fixpoint of the general type dependencies
    fixpoint :: [TypeAnalysis] -> [TypeAnalysis]
    fixpoint ts = let ts' = map (fpPerType ts) ts
                  in if ts == ts' then ts else fixpoint ts'
      where
        fpPerType :: [TypeAnalysis] -> TypeAnalysis -> TypeAnalysis
        fpPerType ts (tn, poss) = (tn, map (fpPerVar ts) poss)

        fpPerVar :: [TypeAnalysis] -> (CTVarIName, TypeDependencies, [VarOccurrence]) -> (CTVarIName, TypeDependencies, [VarOccurrence])
        fpPerVar ts (vn, gdep, occs) = (vn, nub $ gdep ++ newDeps, occs)
          where
            newDeps = concatMap (\tn -> case findType tn ts of
                                              Just (_, poss) -> concatMap (\(_, gdep, _) -> gdep) poss
                                              Nothing        -> []) (nub $ map fst gdep)
            findType tn = find (\(tn', _) -> tn' == tn)

--- Appends some position prefix to a list of positions
appendPos :: Int -> [(VarPos, a)] -> [(VarPos, a)]
appendPos i = map (\(pos, dep) -> (i:pos, dep))

--- Adds a type dependency to a list of variable occurrences
addDependency :: TypeDependency -> [(VarPos, TypeDependencies)] -> [(VarPos, TypeDependencies)]
addDependency (tn, i) = map (\(pos, occ) -> (pos, (tn, i) : occ)) 

--- Result of analyzing a type declaration (`moduleVarPos`).
--- The result, for a specific type name, is a list of all type variables, their general dependencies and their occurrences.
--- A variables general dependency is a disjunction of all (direct and indirect) dependencies of all occurrences of the variable. 
type TypeAnalysis = (Typename, [(CTVarIName, TypeDependencies, [VarOccurrence])])

--- A variable occurrence is a list of all positions where a specific type variable occurs in a constructor declaration.
--- All type variable positions have known type dependencies (depends on a type <=> 'reaching' the variable requires pattern-matching an instance of the type)
type VarOccurrence = (QName, [(VarPos, TypeDependencies)])

--- Used to detect cyclic type dependencies.
--- 
--- For 
---   data A a = A (B a) 
---          ^
---   data B b = B b
---
--- The marked type variable has one type dependency, (B, 0), because in order to reach the type variable,
--- one has to bind the type variable a to the first (0th) polymorphic type variable of B.
--- 
--- If we want to run 'typeOf' on the type variable, we need to make sure that 'typeOf', applied to a term of type B, 
--- doesn't try to run 'typeOf' on a term of type A, which would lead to an infinite loop.
type TypeDependency = (Typename, Int)
type TypeDependencies = [TypeDependency]

type Typename = QName

{- 
-- Generator for the 'someVal' function. 'someVal' returns some arbitrary (possibly cyclic) value of the given type.
stringSomeVal :: FunctionGenerator
stringSomeVal typedecl = do
  Naming{rwBaseModuleName=baseMod} <- getNaming
  if null $ typeCons typedecl
    then do 
      logError $ "Cannot generate 'someVal' for empty type " ++ show (typeDeclToName typedecl)
      return [simpleRule [] (CSymbol ("Prelude", "undefined"))]
    else do 
      let cons0 = head $ typeCons typedecl
      return [simpleRule [] (applyF (consName cons0) $ map (const $ CSymbol (baseMod, "someVal")) [1..(consArity cons0)])]

-- Generator for the 'typeOf' function. 'typeOf' returns the type of the given value.
stringTypeOf :: FunctionGenerator
stringTypeOf typedecl = do
  Naming{rwBaseModuleName=baseMod} <- getNaming
  tas <- getTypeAnalyses
  let ass = completeAssignment tas typedecl
  inExpr' <- inExpr baseMod ass
  if null ass 
    then return [simpleRule [anonPattern] inExpr']
    else do
      return [simpleRule [CPVar (0, "n")] (CLetDecl [letExpr baseMod ass] inExpr')]
  where
    createPattern (Assignment cn assVars) = CPComb cn $ map makePattern [0..(consArity cons - 1)]
      where
        poss = map fst assVars 
        cons = theCons cn typedecl
        makePattern i | i `elem` poss = CPVar (i, "x" ++ snd cn ++ (show i) ++ "'") 
                      | otherwise     = anonPattern

    createCall baseMod (Assignment cn _) = applyF cn $ replicate (consArity cons) (CSymbol (baseMod, "someVal"))
      where 
        cons = theCons cn typedecl

    createPolyCall baseMod (Just (typevar, (at, cn, pos))) = do 
      if (null at) 
        then return $ callTypeOf 
        else return $ applyF (baseMod, "typeAt") [list2ac (map cInt at), callTypeOf]
      where
        callTypeOf = applyF (baseMod, "typeOf") [CVar (pos, "x" ++ snd cn ++ (show pos) ++ "'")]
    createPolyCall baseMod Nothing = do  
      logIllTypedDefinition $ (snd . typeName) typedecl
      return $ applyF (baseMod, "mono") [string2ac "_"]

    letExpr baseMod ass = CLocalPat pat rhs
      where 
        pat = listPattern $ anonPattern : map createPattern ass
        rhs = simpleRhs $ list2ac $ CVar (0, "n") : map (createCall baseMod) ass
    
    inExpr baseMod ass = do
      calls <- polyCalls 
      if null calls
        then return $ applyF (baseMod, "mono") [string2ac (snd $ typeName typedecl)]
        else return $ applyF (baseMod, "RWType") [string2ac (snd $ typeName typedecl), list2ac calls]
      where 
        polyCalls = mapM (\tv -> createPolyCall baseMod (find (\n -> fst n == tv) typeAccs)) (typeVars typedecl)
        typeAccs = assignments2typeAccess ass -}