{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use if" #-}

-- comp2209 Functional Programming Challenges
-- (c) University of Southampton 2021
-- Skeleton code to be updated with your solutions
-- The dummy functions here simply return an arbitrary value that is usually wrong

-- DO NOT MODIFY THE FOLLOWING LINES OF CODE
module Challenges
  ( TileEdge (..),
    Tile (..),
    Puzzle,
    isPuzzleComplete,
    Rotation (..),
    solveCircuit,
    LExpr (..),
    Bind (..),
    prettyPrint,
    parseLetx,
    LamExpr (..),
    letEnc,
    compareRedn,
  )
where

-- Import standard library and parsing definitions from Hutton 2016, Chapter 13

import Data.List (find)
import Data.Maybe (fromJust)
import Debug.Trace (trace)
import Parsing

-- Challenge 1
-- Testing Circuits

data TileEdge = North | East | South | West deriving (Eq, Ord, Show, Read)

data Tile = Source [TileEdge] | Sink [TileEdge] | Wire [TileEdge] deriving (Eq, Show, Read)

type Puzzle = [[Tile]]

type PuzzleWithPos = [[TileWithPos]]

type TileWithPos = (Pos, Tile)

type Pos = (Int, Int)

isPuzzleComplete :: Puzzle -> Bool
isPuzzleComplete puzzle =
  let puzzleWithPos = addPosToPuzzle puzzle
      posMap = concat puzzleWithPos
      lengthAndWith = getLengthAndWidth puzzle
      res = checkAllConnected posMap lengthAndWith
   in res

type PosMap = [(Pos, Tile)]

type LengthAndWidth = (Int, Int)

checkAllConnected :: PosMap -> LengthAndWidth -> Bool
checkAllConnected posMap lengthAndWidth =
  let connected = connect posMap lengthAndWidth [(1, 1)] [(1, 1)]
      res = length connected == uncurry (*) lengthAndWidth
   in res

connect :: PosMap -> LengthAndWidth -> [Pos] -> [Pos] -> [Pos]
connect posMap lengthAndWidth posList connectedPosList =
  case posList of
    [] -> connectedPosList
    x : xs ->
      let neighbors = getUpDownLeftRight lengthAndWidth x
          neighbors2 = filter (\(_, pos) -> pos `notElem` connectedPosList) neighbors
          neighbors3 = filter (canConnect posMap x) neighbors2
          neighbors4 = map snd neighbors3
          newPosList = (neighbors4 ++ xs)
          newConnectedPosList = (neighbors4 ++ connectedPosList)
       in connect posMap lengthAndWidth newPosList newConnectedPosList

canConnect :: PosMap -> Pos -> RelativePos -> Bool
canConnect posMap pos1 (direction, pos2) =
  let tile1 = fromJust $ lookup pos1 posMap
      tile2 = fromJust $ lookup pos2 posMap
   in twoTilesCanConnect tile1 tile2 direction

tileToList :: Tile -> [TileEdge]
tileToList tile =
  case tile of
    Source xs -> xs
    Sink xs -> xs
    Wire xs -> xs

twoTilesCanConnect :: Tile -> Tile -> Direction -> Bool
twoTilesCanConnect t1 t2 direction =
  let edges1 = tileToList t1
      edges2 = tileToList t2
   in case direction of
        U -> elem North edges1 && elem South edges2
        D -> elem South edges1 && elem North edges2
        L -> elem West edges1 && elem East edges2
        R -> elem East edges1 && elem West edges2

type RelativePos = (Direction, Pos)

data Direction = U | D | L | R

getUpDownLeftRight :: LengthAndWidth -> Pos -> [RelativePos]
getUpDownLeftRight (len, wid) (x, y) =
  let pos1 = (U, (x - 1, y))
      pos2 = (D, (x + 1, y))
      pos3 = (L, (x, y - 1))
      pos4 = (R, (x, y + 1))
      posList = [pos1, pos2, pos3, pos4]
   in filter posInLengthAndWidth posList
  where
    posInLengthAndWidth :: RelativePos -> Bool
    posInLengthAndWidth (_, (x, y)) =
      x >= 1 && x <= len && y >= 1 && y <= wid

getLengthAndWidth :: [[a]] -> (Int, Int)
getLengthAndWidth a@(b : _) = (length a, length b)
getLengthAndWidth [] = (0, 0)

addPosToPuzzle :: Puzzle -> PuzzleWithPos
addPosToPuzzle puzzle = map addPosToOneLine $ zip [1 .. n] puzzle where n = length puzzle

addPosToOneLine :: (Int, [Tile]) -> [TileWithPos]
addPosToOneLine (n, line) =
  let posList = map (\x -> (n, x)) [1 .. m] where m = length line
      res = zip posList line
   in res

-- Challenge 2
-- Solving Circuits
data Rotation = R0 | R90 | R180 | R270
  deriving (Eq, Show, Read)

solveCircuit :: Puzzle -> Maybe [[Rotation]]
solveCircuit puzzle =
  let posMap = concat $ addPosToPuzzle puzzle
      lengthAndWidth = getLengthAndWidth puzzle
      maybeSolution = tryFindSolution posMap lengthAndWidth
   in case maybeSolution of
        Just solution -> Just $ foldTo2D wid solution where wid = snd lengthAndWidth
        Nothing -> Nothing

foldTo2D :: Int -> [a] -> [[a]]
foldTo2D _ [] = []
foldTo2D n xs =
  let (chunk, rest) = splitAt n xs
   in chunk : foldTo2D n rest

tryFindSolution :: PosMap -> LengthAndWidth -> Maybe [Rotation]
tryFindSolution posMap lengthAndWidth =
  let len = length posMap
      allPossibleRotationList = cartesianProduct $ map (const [R0, R90, R180, R270]) [1 .. len]
      allPossiblePosMap = map (applyRotations . (\x -> (posMap, x))) allPossibleRotationList
      rotaionListPosMapTupleList = zip allPossibleRotationList allPossiblePosMap
   in case find (\(rotaionList, posMap) -> checkAllConnected posMap lengthAndWidth) rotaionListPosMapTupleList of
        Just (r, m) -> Just r
        Nothing -> Nothing

applyRotations :: (PosMap, [Rotation]) -> PosMap
applyRotations (posMap, rotations) =
  zipWith applyRotation posMap rotations

applyRotation :: (Pos, Tile) -> Rotation -> (Pos, Tile)
applyRotation (pos, tile) rotation =
  let newTile = applyFunToEdges (map (applyRotationToEdge rotation)) tile
   in (pos, newTile)

applyFunToEdges :: ([TileEdge] -> [TileEdge]) -> Tile -> Tile
applyFunToEdges f tile =
  case tile of
    Source xs -> Source $ f xs
    Sink xs -> Sink $ f xs
    Wire xs -> Wire $ f xs

applyRotationToEdge :: Rotation -> TileEdge -> TileEdge
applyRotationToEdge rotation edge =
  intToTileEdge (rotationToInt rotation + tileEdgeToInt edge)

rotationToInt :: Rotation -> Integer
rotationToInt rotation =
  case rotation of
    R0 -> 0
    R90 -> 90
    R180 -> 180
    R270 -> 270

tileEdgeToInt :: TileEdge -> Integer
tileEdgeToInt edge =
  case edge of
    North -> 270
    South -> 90
    West -> 180
    East -> 0

intToTileEdge :: Integer -> TileEdge
intToTileEdge int =
  case int `mod` 360 of
    270 -> North
    90 -> South
    180 -> West
    0 -> East

cartesianProduct :: [[a]] -> [[a]]
cartesianProduct [] = [[]]
cartesianProduct (xs : xss) = [x : ys | x <- xs, ys <- cartesianProduct xss]

-- Challenge 3
-- Pretty Printing Let Expressions

data LExpr = Var Int | App LExpr LExpr | Let Bind LExpr LExpr | Pair LExpr LExpr | Fst LExpr | Snd LExpr | Abs Bind LExpr
  deriving (Eq, Show, Read)

data Bind = Discard | V Int
  deriving (Eq, Show, Read)

prettyPrint :: LExpr -> String
prettyPrint (App arg1 arg2) = mayAddParen arg1 ++ " " ++ prettyPrint arg2
prettyPrint (Let arg1 arg2 arg3) =
  let (bindsTail, expr1) = getBindsTailAndExpr arg2
      binds = arg1 : bindsTail
   in "let " ++ printBinds binds ++ " = " ++ prettyPrint expr1 ++ " in " ++ prettyPrint arg3
prettyPrint (Abs arg1 arg2) =
  let (bindsTail, expr1) = getBindsTailAndExpr arg2
      binds = arg1 : bindsTail
   in "\\" ++ printBinds binds ++ " -> " ++ prettyPrint expr1
prettyPrint (Var int) = "x" ++ show int
prettyPrint (Pair arg1 arg2) = "(" ++ prettyPrint arg1 ++ " , " ++ prettyPrint arg2 ++ ")"
prettyPrint (Fst arg) = "fst " ++ "(" ++ prettyPrint arg ++ ")"
prettyPrint (Snd arg) = "snd " ++ "(" ++ prettyPrint arg ++ ")"

getBindsTailAndExpr :: LExpr -> ([Bind], LExpr)
getBindsTailAndExpr expr = f expr []
  where
    f expr binds = case expr of
      Abs arg1 arg2 -> f arg2 (arg1 : binds)
      other -> (reverse binds, other)

printBind bind = case bind of
  Discard -> "_"
  V int -> "x" ++ show int

printBinds :: [Bind] -> String
printBinds [bind] = printBind bind
printBinds (x : xs) = printBind x ++ " " ++ printBinds xs

needParen :: LExpr -> Bool
needParen expr = case expr of
  Let {} -> True
  Abs {} -> True
  _ -> False

mayAddParen :: LExpr -> String
mayAddParen x = case needParen x of
  True -> "(" ++ prettyPrint x ++ ")"
  False -> prettyPrint x

-- Challenge 4 - Parsing Let Expressions
-- parseExpr

parseLetx :: String -> Maybe LExpr
parseLetx text =
  let parseResult = parse expr text
   in case parseResult of
        [(lExpr, "")] -> Just lExpr
        _ -> Nothing

-- >>> parseLetx "x1 x2 x3       "
-- Just (App (App (Var 1) (Var 2)) (Var 3))
expr =
  do
    exprList <- some expr'
    let x : xs = exprList
    return $ foldl App x xs

expr' =
  var
    <|> letExpr
    <|> parenExpr
    <|> pairExpr
    <|> fstExpr
    <|> sndExpr
    <|> lambdaExpr

sndExpr =
  do
    symbol "snd"
    symbol "("
    expr1 <- expr
    symbol ")"
    return $ Snd expr1

lambdaExpr =
  do
    symbol "\\"
    varList1 <- varList
    symbol "->"
    expr1 <- expr
    return $ buildLambdaExpr varList1 expr1

buildLambdaExpr :: [Bind] -> LExpr -> LExpr
buildLambdaExpr binds lExpr = foldr Abs lExpr binds

fstExpr =
  do
    symbol "fst"
    symbol "("
    expr1 <- expr
    symbol ")"
    return $ Fst expr1

letExpr =
  do
    symbol "let"
    eqn1 <- eqn
    symbol "in"
    expr1 <- expr
    return $ buildLetExpr eqn1 expr1

buildLetExpr :: ([Bind], LExpr) -> LExpr -> LExpr
buildLetExpr (binds, lExpr1) lExpr2 =
  let arg1 = head binds
      arg2 = foldr Abs lExpr1 (tail binds)
      arg3 = lExpr2
   in Let arg1 arg2 arg3

parenExpr =
  do
    symbol "("
    expr1 <- expr
    symbol ")"
    return expr1

pairExpr =
  do
    symbol "("
    expr1 <- expr
    symbol ","
    expr2 <- expr
    symbol ")"
    return $ Pair expr1 expr2

eqn = do
  varList1 <- varList
  symbol "="
  expr1 <- expr
  return (varList1, expr1)

varList = some varB

-- >>>parse varB "x1"
-- [(V 1,"")]

-- >>>parse varB " _ "
-- [(Discard,"")]
varB =
  token $
    do
      char 'x'
      v <- digits
      let int = read v :: Int
      return $ V int
      <|> do
        symbol "_"
        return Discard

-- >>>parse (some var) " x1 x2 x3"
-- [([Var 1,Var 2,Var 3],"")]
var = token $ do
  char 'x'
  v <- digits
  let int = read v :: Int
  return $ Var int

digits = some digit

-- >>>parse (varB ) " _ "
-- [("_","")]

-- Challenge 5
-- Let Encoding in Lambda
data LamExpr = LamVar Int | LamApp LamExpr LamExpr | LamAbs Int LamExpr
  deriving (Eq, Show, Read)

letEnc :: LExpr -> LamExpr
letEnc expr = encLExpr expr ([], 0)

type VarNum = Int

type VarMap = [(VarNum, VarNum)]

type Ctx = (VarMap, VarNum)

encBind :: Bind -> Ctx -> (Int, Ctx)
encBind bind (varMap, varNum) =
  case bind of
    Discard -> (varNum, (varMap, varNum + 1))
    V int -> (varNum, ((int, varNum) : varMap, varNum + 1))

encLExpr :: LExpr -> Ctx -> LamExpr
encLExpr (App arg1 arg2) ctx =
  let x1 = encLExpr arg1 ctx
      x2 = encLExpr arg2 ctx
   in LamApp x1 x2
encLExpr (Let e0 e1 e2) ctx =
  let (x0, ctx0) = encBind e0 ctx
      x1 = encLExpr e1 ctx
      x2 = encLExpr e2 ctx0
      lamExpr = LamApp (LamAbs x0 x2) x1
   in lamExpr
encLExpr (Abs bind e1) ctx =
  let (xN, ctx1) = encBind bind ctx
   in LamAbs xN (encLExpr e1 ctx1)
encLExpr (Var varNum) (varMap, _) =
  let lamVarNum = lookup varNum varMap
   in case lamVarNum of
        Just int -> LamVar int
        Nothing -> LamVar $ varNum + 1
encLExpr (Pair e1 e2) (varMap, varNum) =
  let ctx1 = (varMap, varNum + 1)
      x1 = encLExpr e1 ctx1
      x2 = encLExpr e2 ctx1
   in LamAbs varNum (LamApp (LamApp (LamVar varNum) x1) x2)
encLExpr (Fst e) ctx@(_, varNum) =
  let lamVarNum0 = varNum
      lamVarNum1 = varNum + 1
      encodedE = encLExpr e ctx
   in LamApp encodedE (LamAbs lamVarNum0 (LamAbs lamVarNum1 (LamVar lamVarNum0)))
encLExpr (Snd e) ctx@(_, varNum) =
  let lamVarNum0 = varNum
      lamVarNum1 = varNum + 1
      encodedE = encLExpr e ctx
   in LamApp encodedE (LamAbs lamVarNum0 (LamAbs lamVarNum1 (LamVar lamVarNum1)))

-- Challenge 6
-- Compare Innermost Reduction for Let_x and its Lambda Encoding

------------
-- LAMBDA --
------------

free :: Int -> LamExpr -> Bool
free x (LamVar y) = x == y
free x (LamAbs y e) | x == y = False
free x (LamAbs y e) | x /= y = free x e
free x (LamApp e1 e2) = (free x e1) || (free x e2)

rename :: Int -> LamExpr -> Int
rename x e
  | free (x + 1) e = rename (x + 1) e
  | otherwise = x + 1

subst :: LamExpr -> Int -> LamExpr -> LamExpr
subst (LamVar x) y e | x == y = e
subst (LamVar x) y e | x /= y = LamVar x
subst (LamAbs x e1) y e | x /= y && not (free x e) = LamAbs x (subst e1 y e)
subst (LamAbs x e1) y e | x /= y && (free x e) = let x' = (rename x e1) in subst (LamAbs x' (subst e1 x (LamVar x'))) y e
subst (LamAbs x e1) y e | x == y = LamAbs x e1
subst (LamApp e1 e2) y e = LamApp (subst e1 y e) (subst e2 y e)

isLamValue :: LamExpr -> Bool
isLamValue (LamVar _) = True
isLamValue (LamAbs _ _) = True
isLamValue _ = False

-- CALL BY VALUE --
cbvlam1 :: LamExpr -> Maybe LamExpr
-- Contexts
cbvlam1 (LamApp e1 e2) | not (isLamValue e1) =
  do
    e' <- cbvlam1 e1
    return (LamApp e' e2)
cbvlam1 (LamApp e1 e2) | not (isLamValue e2) =
  do
    e' <- cbvlam1 e2
    return (LamApp e1 e')
-- Reductions
cbvlam1 (LamApp (LamAbs x e1) e) | isLamValue e = Just (subst e1 x e)
-- Otherwise terminated or blocked
cbvlam1 _ = Nothing

-- CALL BY NAME --l;
cbnlam1 :: LamExpr -> Maybe LamExpr
-- Reductions
cbnlam1 (LamApp (LamAbs x e1) e) = Just (subst e1 x e)
-- Contexts
cbnlam1 (LamApp e1 e2) =
  do
    e' <- cbnlam1 e1
    return (LamApp e' e2)
-- Otherwise terminated or blocked
cbnlam1 _ = Nothing

---------
-- LET --
---------
freeLet :: Int -> LExpr -> Bool
freeLet x (Var y) = x == y
freeLet x (App e1 e2) = (freeLet x e1) || (freeLet x e2)
freeLet x (Let (V y) e1 e2) | x /= y = (freeLet x e1) || (freeLet x e2)
freeLet x (Let Discard e1 e2) = (freeLet x e1) || (freeLet x e2)
freeLet x (Let (V y) e1 e2) | x == y = False
freeLet x (Pair e1 e2) = (freeLet x e1) || (freeLet x e2)
freeLet x (Fst e) = freeLet x e
freeLet x (Snd e) = freeLet x e
freeLet x (Abs (V y) e) | x == y = False
freeLet x (Abs (V y) e) | x /= y = freeLet x e
freeLet x (Abs Discard e) = freeLet x e

isLetValue :: LExpr -> Bool
isLetValue (Var _) = True
isLetValue (App (Abs {}) _) = False
isLetValue (App e1 e2) = isLetValue e1 && isLetValue e2
isLetValue (Let {}) = False
isLetValue (Pair e1 e2) = isLetValue e1 && isLetValue e2
isLetValue (Fst (Pair {})) = False
isLetValue (Fst e) = isLetValue e
isLetValue (Snd (Pair {})) = False
isLetValue (Snd e) = isLetValue e
isLetValue (Abs _ e) = isLetValue e

renameLet :: Int -> LExpr -> Int
renameLet x e
  | freeLet (x + 1) e = renameLet (x + 1) e
  | otherwise = x + 1

substLet :: LExpr -> Int -> LExpr -> LExpr
substLet expr y e =
  -- trace ("substLet: " ++ show expr ++ "  y:" ++ show y ++ " e: " ++ show e) $
  case expr of
    (Var x) | x == y -> e
    (Var x) | x /= y -> Var x
    (App e1 e2) -> App (substLet e1 y e) (substLet e2 y e)
    (Let Discard e1 e2) -> Let Discard (substLet e1 y e) (substLet e2 y e)
    (Let (V x) e1' e2) ->
      let e1 = substLet e1' y e
       in case "guard" of
            _
              | x /= y && not (freeLet x e) -> Let (V x) e1 (substLet e2 y e)
              | x /= y && (freeLet x e) -> let x' = renameLet x e2 in substLet (Let (V x') e1 (substLet e2 x (Var x'))) y e
              | x == y -> Let (V x) e1 e2
    (Pair e1 e2) -> Pair (substLet e1 y e) (substLet e2 y e)
    (Fst e1) -> Fst (substLet e1 y e)
    (Snd e1) -> Snd (substLet e1 y e)
    (Abs Discard e1) -> Abs Discard (substLet e1 y e)
    (Abs (V x) e1) | x /= y && not (freeLet x e) -> Abs (V x) (substLet e1 y e)
    (Abs (V x) e1) | x /= y && (freeLet x e) -> let x' = renameLet x e1 in substLet (Abs (V x') (substLet e1 x (Var x'))) y e
    (Abs (V x) e1) | x == y -> Abs (V x) e1

cbvlet1 :: LExpr -> Maybe LExpr
cbvlet1 (Var _) = Nothing
cbvlet1 (App e1 e2)
  | not (isLetValue e2) =
      do
        e' <- cbvlet1 e2
        return (App e1 e')
  | not (isLetValue e1) =
      do
        e' <- cbvlet1 e1
        return (App e' e2)
cbvlet1 (App (Abs (V x) e1) e2) = Just (substLet e1 x e2)
cbvlet1 (Let bind e1 e2)
  | not (isLetValue e2) =
      do
        e' <- cbvlet1 e2
        return (Let bind e1 e')
  | not (isLetValue e1) =
      do
        e' <- cbvlet1 e1
        return (Let bind e' e2)
  | otherwise =
      case bind of
        Discard -> Just e2
        (V x) -> Just (substLet e2 x e1)
cbvlet1 (Pair e1 e2)
  | not (isLetValue e2) =
      do
        e' <- cbvlet1 e2
        return (Pair e1 e')
  | not (isLetValue e1) =
      do
        e' <- cbvlet1 e1
        return (Pair e' e2)
cbvlet1 (Fst e) | not (isLetValue e) =
  do
    e' <- cbvlet1 e
    return $ Fst e'
cbvlet1 (Fst (Pair e1 e2)) = Just e1
cbvlet1 (Snd e) | not (isLetValue e) =
  do
    e' <- cbvlet1 e
    return $ Snd e'
cbvlet1 (Snd (Pair e1 e2)) = Just e2
cbvlet1 (Abs bind e2) | not (isLetValue e2) =
  do
    e' <- cbvlet1 e2
    return (Abs bind e')
cbvlet1 (Abs bind e1) | not (isLetValue e1) =
  do
    e' <- cbvlet1 e1
    return $ Abs bind e'
cbvlet1 _ = Nothing

cbnlet1 :: LExpr -> Maybe LExpr
cbnlet1 (App (Abs (V x) e1) e) = Just (substLet e1 x e)
cbnlet1 (App (Abs Discard e1) e) = Just e1
cbnlet1 (App e1 e2)
  | not $ isLetValue e1 = do
      e' <- cbnlet1 e1
      return $ App e' e2
  | not $ isLetValue e2 = do
      e' <- cbnlet1 e2
      return $ App e1 e'
cbnlet1 (Let Discard e1 e2) = Just e2
cbnlet1 (Let (V x) e1 e2) = Just (substLet e2 x e1)
cbnlet1 (Pair e1 e2)
  | not $ isLetValue e1 = do
      e' <- cbnlet1 e1
      return $ Pair e' e2
  | not $ isLetValue e2 = do
      e' <- cbnlet1 e2
      return $ Pair e1 e'
cbnlet1 (Fst (Pair e1 e2)) = Just e1
cbnlet1 (Snd (Pair e1 e2)) = Just e2
cbnlet1 (Abs bind e) =
  do
    e' <- cbnlet1 e
    return $ Abs bind e
cbnlet1 _ = Nothing

numStepsReduction :: (Show a, Show t, Ord a, Num a) => a -> (t -> Maybe t) -> t -> a
numStepsReduction maxStepNum f expr = reduce_ f expr 0 maxStepNum
  where
    reduce_ f expr int maxStepNum =
      -- trace ("\nstep" ++ show int ++ " " ++ show expr) $
      case int <= maxStepNum of
        True -> case f expr of
          Just reducedExpr -> reduce_ f reducedExpr (int + 1) maxStepNum
          Nothing -> int
        False -> maxStepNum

compareRedn :: LExpr -> Int -> (Int, Int, Int, Int)
compareRedn expr maxNum =
  let x1 = numStepsReduction maxNum cbvlet1 expr
      x2 = numStepsReduction maxNum cbvlam1 $ letEnc expr
      x3 = numStepsReduction maxNum cbnlet1 expr
      x4 = numStepsReduction maxNum cbnlam1 $ letEnc expr
   in (x1, x2, x3, x4)
