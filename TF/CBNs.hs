import System.IO.Unsafe
import NewParser

-- Value
data Val = VInt Int
         | VBool Bool
         | VClosure Variable Expr Env
         | VStrictClosure Variable Expr Env -- 正格関数抽象式の評価結果
         deriving (Eq, Show)

showVal :: Val -> [Char]
showVal (VInt m) = show m
showVal (VBool b) = show b
showVal (VClosure v e r) =
  "Closure [lambda " ++ show v ++ " . " ++ show e ++ "]"

unwrapInt :: Val -> Int
unwrapInt (VInt m) = m
upwrapInt _ = error "not an integer"

unwrapBool :: Val -> Bool
unwrapBool (VBool b) = b
upwrapBool _ = error "not a boolean"

-- Thunk
data Thunk = Thunk Expr Env
           deriving (Eq, Show)

-- Environment
type Assoc a b = [(a,b)]

emptyAssoc :: Assoc a b
emptyAssoc = []

lookupAssoc :: (Eq a) => a -> Assoc a b -> Maybe b
lookupAssoc x [] = Nothing
lookupAssoc x ((k,v):ps) | x == k    = Just v
                         | otherwise = lookupAssoc x ps

updateAssoc :: (Eq a) => a -> b -> Assoc a b -> Assoc a b
updateAssoc k v ps = (k,v):ps

data EnvVal = Evaled Val
            | Delayed Thunk
            deriving (Eq, Show)

type Env = Assoc Variable EnvVal

emptyEnv :: Env
emptyEnv = emptyAssoc

lookupEnv :: Variable -> Env -> Maybe EnvVal
lookupEnv v env = lookupAssoc v env

updateEnv :: Variable -> EnvVal -> Env -> Env
updateEnv v envval env = updateAssoc v envval env

-- Evaluation
expval :: Expr -> Env -> Val
expval (Num n) env = n `seq` VInt n
expval (Var x) env = getval x (lookupEnv x env)
expval (Bexpr o e1 e2) env =
  v1 `seq` v2 `seq` r `seq` VInt r
  where v1 = expval e1 env
        v2 = expval e2 env
        r = binop o (unwrapInt v1) (unwrapInt v2)
expval (Rexpr o e1 e2) env =
  v1 `seq` v2 `seq` r `seq` VBool r
  where v1 = expval e1 env
        v2 = expval e2 env
        r = relop o (unwrapInt v1) (unwrapInt v2)
expval (Fun x e) env = VClosure x e env
expval (SFun x e) env = VStrictClosure x e env -- 正格関数抽象式の評価結果
expval (Apply e1 e2) env =
  case expval e1 env of
    VClosure x body env' -> expval body newenv
      where newenv = updateEnv x (Delayed (Thunk e2 env)) env' -- 遅延評価
    VStrictClosure x body env' -> expval body newenv -- 正格評価
      where newenv = updateEnv x (Evaled v2) env'
            v2 = expval e2 env
    _ -> error "not a function"

expval (Let decls e2) env =
   v `seq` expval e2 newenv
   where
       (Decl x e1) = head decls
       v = delay e1 env
       newenv = updateEnv x v env

expval (Letrec decls e2) env =
   v `seq` expval e2 newenv
   where
       (Decl x e1) = head decls
       v = delay e1 newenv
       newenv = updateEnv x v env

expval (If e1 e2 e3) env =
  v `seq` expval (if unwrapBool v then e2 else e3) env
  where v = expval e1 env
expval (Pr e) env =
  unsafePerformIO $ do { putStrLn (showVal v); return v;}
  where v = expval e env

getval :: Variable -> Maybe EnvVal -> Val
getval var (Just envval) = forceVal envval
getval var Nothing = error ("getval: " ++ var)

forceVal :: EnvVal -> Val
forceVal (Evaled v) = v
forceVal (Delayed (Thunk exp env)) = expval exp env

delay :: Expr -> Env -> EnvVal
delay exp env = Delayed (Thunk exp env)

binop :: BinOp -> Int -> Int -> Int
binop Add = (+)
binop Sub = (-)
binop Mul = (*)
binop Div = div

relop :: RelOp -> Int -> Int -> Bool
relop Equal = (==)
relop NotEqual = (/=)
relop LessThan = (<)
relop LessThanEqual = (<=)

ev :: [Char] -> Val
ev s = expval (parseProg s) emptyEnv

