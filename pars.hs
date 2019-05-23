import qualified Control.Monad as M
import           Data.Char
import           System.IO

-- fixed C source header
header = "#include <stdio.h>\n#include <stdint.h>\n\ntypedef uint64_t u;\n\n" ++ "u _ssharp_write(u _input) {\n\t printf(\"%lu\\n\", _input);\n}\n" ++ "u _ssharp_read() {\n\tu _tmp;\n\tscanf(\"%lu\", &_tmp);\n\treturn _tmp;\n}\n"

alpha = "abcdefghijklmnopqrstuvwxyz"
num = "0123456789"

-- list of operations sorted by priority (first are the lowest priority)
priorityList = [[("||", Or)], [("&&", And)], [("==", Eq), ("!=", NotEq)], [(">", Gt), ("<", Lt)], [("+", Plus), ("-", Minus)], [("*", Mult), ("/", Div), ("%", Mod)]]

reservedNames = ["if"]

main = do 
    source <- getContents
    let res = runParser pSource source
        in case res of
            Just (a,"") -> putStrLn (header ++ show a)
            Nothing -> putStrLn "Parsing failed"
            Just (a, b) -> putStrLn ("Partial parsing success:\n " ++ show a ++ "\n\n\n Unparsed text: \n" ++  b)


newtype EList = EList [Expr]
newtype Params = Params [Identifier]
newtype Source = Source [FuncDef]
newtype Identifier = Identifier String deriving Eq

data FuncDef = FuncDef Identifier Params Expr

data Expr = Const Int | Un Unop Expr | Bi Binop Expr Expr | List EList | If Expr Expr Expr | FCall Identifier EList | Var Identifier | Prog EList

data Unop = UMinus | Not
data Binop = Eq | NotEq | Lt | Gt | Plus | Minus | Mult | Div | Mod | Or | And

instance Show Source where
    show (Source []) = ""
    show (Source (f:fs)) = show f ++ "\n" ++ show (Source fs)

instance Show FuncDef where
    show (FuncDef name params src) = datatype ++ " " ++ show name ++ "(" ++ show params ++ ")" ++ " {\n" ++ "\treturn " ++ show src ++ ";\n}\n"
        where datatype = "u" -- if name == (Identifier "main") then "int" else "u"

instance Show Identifier where
    show (Identifier name)
        | name == "main" = "main"
        | otherwise      = "_ssharp_" ++ name

instance Show Params where
    show (Params []) = ""
    show (Params [p]) = "u " ++ show p
    show (Params (p:ps)) = "u " ++ show p ++ "," ++ show (Params ps)

instance Show Unop where
    show UMinus = "-"
    show Not = "!"

instance Show EList where
    show (EList []) = ""
    show (EList [x]) = show x
    show (EList (x:xs)) = show x ++ ", " ++ show (EList xs)

instance Show Expr where
    show (Const a) = show a
    show (Un op e) = show op ++ show e
    show (Bi op l r) = "(" ++ show l ++ " " ++ show op ++ " " ++ show r ++ ")"
    show (List xs) = "(" ++ show xs ++ ")"
    show (If c i e) = "((" ++ show c ++ ")" ++ " ?\n\t" ++ show i ++ " : \n\t" ++ show e ++ ")"
    show (FCall name xs) = show name ++ "(" ++ show xs ++ ")"
    show (Var name) = show name
    show (Prog xs) = "(" ++ show xs ++ ")"

instance Show Binop where
    show Eq = "=="
    show NotEq = "!="
    show Lt = "<"
    show Gt = ">"
    show Plus = "+"
    show Minus = "-"
    show Mult = "*"
    show Div = "/"
    show Mod = "%"
    show Or = "||"
    show And = "&&"

-- Own monadic parser, Just(parsed_result, remaining_text) on success, Nothing on failure
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- parses one char depending on given predicate
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser (\input -> if null input
                            then Nothing
                            else if p (head input) then Just (head input, tail input) else Nothing)
-- always fails
failure :: Parser a
failure = Parser (\_ -> Nothing)

-- tries to use p1 otherwise uses p2
orElse :: Parser a -> Parser a -> Parser a
orElse p1 p2 = Parser (\input ->
    case runParser p1 input of 
        Just a -> Just a
        otherwise -> runParser p2 input)


parserReturn :: a -> Parser a
parserReturn x = Parser (\input -> Just (x, input))

-- >>= implementation: uses parser m, forwards results to f on success, otherwise fails
parserBind :: Parser a -> (a -> Parser b) -> Parser b
parserBind m f = Parser (\input -> case runParser m input of
                                    Just (a,rest) -> runParser (f a) rest
                                    otherwise -> Nothing)

instance Functor Parser where
    fmap = M.liftM

instance Applicative Parser where
    pure  = return
    (<*>) = M.ap

instance Monad Parser where
    return = parserReturn
    (>>=)  = parserBind

-- tries to parse given string
string :: String -> Parser String
string [] = do
    return ""
string (x:xs) = do
    a <- satisfy (==x)
    b <- string xs
    return (a:b)

--tries to parse given string maybe surrounded by spaces on the left/right side
--(returns the string without consumed spaces)
wstring :: Bool -> Bool -> String -> Parser String
wstring l r s= do
    if l then optWhitespace else return ()
    string s
    if r then optWhitespace else return ()
    return s

-- uses given parser 0 or more times
many :: Parser a -> Parser [a]
many p = do
    (some p `orElse` return [])

-- uses given parser 1 or more times
some :: Parser a -> Parser [a]
some p = do
    a <- p
    b <- many p
    return (a:b)

-- tries to use given parsers one after another
oneOf :: [Parser a] -> Parser a
oneOf [] = failure
oneOf [x] = x
oneOf (x:xs) = do
    x `orElse` oneOf xs

pInt :: Parser Expr
pInt = do
    str <- some (satisfy (`elem` num))
    return (Const (read str))

-- tries to parse one of given operations (returns corresponding Binop)  
pOp :: [(String, Binop)] -> Parser Binop
pOp [] = failure
pOp ((name,op):os) = do
    (do
        a <- string name
        return op)
    `orElse`
    pOp os

-- identifier starts with _ or a-z and is followed by 0 or more [_a-z0-9]
-- fails if identifier is from reserved names
identifier :: Parser Identifier
identifier = do
    f <- satisfy (`elem` ('_':alpha))
    r <- many (satisfy (`elem` ("_" ++ alpha ++ num)))
    if (f:r) `elem` reservedNames then failure else return (Identifier (f:r))

-- if expression = if (expr) {prog} {prog}
pIf :: Parser Expr
pIf = do
    string "if"
    wstring True True "("
    cond <- expr
    wstring True True ")"
    wstring True True "{"
    true <- pProg
    wstring True True "}"
    wstring True True "{"
    false <- pProg
    wstring True True "}"
    return (If cond true false)

-- source = sequence of function definitions
pSource :: Parser Source
pSource = do
    fs <- some (do
        f <- pFuncDef
        optWhitespace
        return f)
    return (Source fs)

-- basic expression 
basic :: Parser Expr
basic = oneOf [
    do -- negated basic
        string "-"
        e <- basic
        return (Un UMinus e),
    do -- logically negated basic
        string "~"
        e <- basic
        return (Un Not e),
    pInt, -- integer constant
    pFCall, -- function call
    do -- variable 
        name <- identifier
        return (Var name),
    pIf,
    do -- expression in brackets = (expr)
        wstring False True "("
        e <- expr
        wstring True False ")"
        return e,
    do -- {expr1; expr2; ..}  
        wstring False True "{"
        e <- pProg
        wstring True False "}"
        return e
    ]


expr = expr' priorityList

-- Parses expression given operator priorities
expr':: [[(String, Binop)]] -> Parser Expr
expr' [] = basic
expr' (ops:rest) = do
    c <- expr' rest
    r <- many (do
            optWhitespace
            op <- pOp ops
            optWhitespace
            d <- expr' rest
            return (op,d)
            )
    return (lAssoc ((undefined,c):r))


-- structures expression left-associatively given array of operators and operands
-- [(undefined, 1), (Plus, 3), (Minus, 5)] -> ((1 + 3) - 5)
-- first operator is not used (all LHS operands' operators in fold)
lAssoc :: [(Binop, Expr)] -> Expr
lAssoc = snd . foldl1 (\(_,a) (op, b) -> (undefined, Bi op a b))

-- parses semicolon delimetered sequence of expressions
pProg :: Parser Expr
pProg = do
    e <- expr
    optWhitespace
    es <- many (do
        wstring False True ";"
        x <- expr
        optWhitespace
        return x)
    return (Prog (EList (e:es)))
        
-- function definition = name a b c { prog }
pFuncDef :: Parser FuncDef
pFuncDef = do
    name <- identifier
    params <- many (do
        whitespace
        identifier)
    wstring True True "{"
    src <- pProg
    wstring True True "}"
    return (FuncDef name (Params params) src)

-- function call = name(expr, expr, expr)
pFCall :: Parser Expr
pFCall = do
    name <- identifier
    wstring True True "("
    params <- (
        (do
            string ")"
            return [])
        `orElse` (do
                        p <- expr
                        optWhitespace
                        ps <- many (do
                                        wstring False True ","
                                        r <- expr
                                        return r)
                        string ")"
                        return (p:ps)))
    return (FCall name (EList params))

-- parses 0 or more whitespaces
optWhitespace :: Parser ()
optWhitespace = do
    many $ satisfy isSpace
    return ()

-- at least one whitespace
whitespace :: Parser ()
whitespace = do
    some $ satisfy isSpace
    return ()
