
-- Copyright (c) 2017 John Lång (john.larry.lang@cern.ch)
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.
--
-- |CombinatoryCalculator is a simple DSL for interpreting SKI-combinatory
-- calculus using normal reduction order and parentheses-free notation. The main
-- idea is to use alphabet {A,S,K,I} for reprsenting the S, K, and I combinators
-- and an application operator A. Expressions are represented as strings (a.k.a.
-- lists) of these symbols using infix notation. An applicative expression in
-- SKI-combinatory calculus of the form "(e f)" translates into [A,e,f].
module CombinatoryCalculator
(
    Symbol(..), Expression, ParseTree, FancyTree, Command(..),
    fromString, fromParseTree, tryStepNList
)
where

    import Data.List
    import Debug.Trace

    -- |The language of this system contains three symbols: The combinator
    -- symbols S, K, and I, and the application symbol A.
    data Symbol         = -- |This symbol is used for expressing the application
                          -- operator.
                          A
                          -- |This is the symbol for S combinator.
                        | S
                          -- |This is the symbol for K combinator.
                        | K
                          -- |This is the symbol for I combinator.
                        | I
                        deriving Eq

    combinator          :: Symbol -> Bool
    combinator A        = False
    combinator _        = True

    -- |Expressions of this language are strings of the four symbols.
    type Expression     = [Symbol]

    -- |Converts a Char to a Symbol, if possible.
    fromChar    :: Char -> Maybe Symbol
    fromChar c  = case c of
        '*'     -> Just A
        'S'     -> Just S
        'K'     -> Just K
        'I'     -> Just I
        _       -> Nothing

    -- TODO: Figure out how Read works. This is not very beautiful as it is:
    -- |A helper function for reading a string as an Expression. Symbols other
    -- than '*', 'S', 'K', or 'I' will be silently discarded.
    fromString          :: String -> Expression
    fromString s        =
        let g x y       = case fromChar x of Just z -> z:y; Nothing -> y
        in foldr g [] s

    instance Show Symbol where
        show A          = "*"
        show S          = "S"
        show K          = "K"
        show I          = "I"
        showList []     = id
        showList (x:xs) = \y -> (show x) ++ (showList xs y)

    -- |ParseTree is a simple binary tree representation for a well-formed
    -- Expression. An Expression is well-formed if and only if every application
    -- operator in it has exactly two operands.
    data ParseTree      = -- |An Atom corresponds with one of the expressions
                          -- "S", "K", or "I".
                          Atom Symbol
                          -- |App node has exactly two subtrees.
                        | App ParseTree ParseTree
                        deriving Eq

    -- |Converts a ParseTree to an Expression.
    fromParseTree           :: ParseTree -> Expression
    fromParseTree (Atom x)  = [x]
    fromParseTree (App t u) = A : (fromParseTree t) ++ (fromParseTree u)

    -- |Shows the ParseTree as an expression of classical combinatory calculus
    -- with minimal number of parentheses.
    instance Show ParseTree where
        show (Atom x)           = show x
        show (App t (Atom x))   = (show t) ++ (show x)
        show (App t u)          = (show t) ++ "(" ++ (show u) ++ ")"

    -- |Transforms an Expresion into a ParseTree, if possible. Only well-formed
    -- Expressions have parse trees.
    parse               :: Expression -> [ParseTree] -> Maybe ParseTree
    parse (x:input) stack
        | combinator x  = parse input ((Atom x) : stack)
        | otherwise     = case stack of
            (t:u:rest)  -> parse input ((App t u):rest)
            _           -> Nothing
    parse [] [t]        = Just t
    parse _ _           = Nothing

    -- |A helper function that converts an Expression into a ParseTree, if
    -- possible.
    tryParse            :: Expression -> Maybe ParseTree
    tryParse e          = parse (reverse e) []

    -- |FancyTree is a ParseTree that has a more complex textual presentation.
    -- Extra information is included in nodes for rendering.
    data FancyTree      = FAtom [Bool] Symbol | FApp [Bool] FancyTree FancyTree

    instance Show FancyTree where
        show (FAtom _ x)    = " " ++ (show x)
        show (FApp os t u)  =
            let f = (\(w,m) -> (w ++ (if os !! m then "│ " else "  "), m+1));
                indent os   = fst ((iterate f ("", 0)) !! (length os));
            in  "┬─" ++ (show t) ++ "\n" ++ indent os ++ "└─" ++ (show u)

    -- |Transforms a ParseTree into a FancyTree.
    decorate                :: ParseTree -> [Bool] -> FancyTree
    decorate (App t u) os   = FApp os (decorate t (os ++ [True]))
                                (decorate u (os ++ [False]))
    decorate (Atom x) os    = FAtom os x

    -- |A helper function for transforming Expressions into FancyTrees.
    tryDecorate         :: Expression -> Maybe FancyTree
    tryDecorate e       = case tryParse e of
        Just t          -> Just (decorate t [])
        _               -> Nothing

    -- |True if and only if the ParseTree represents an expression of the form
    -- (using the classical notation):
    -- 1. S e f g;
    -- 2. K e f; or
    -- 3. I e
    redex :: ParseTree -> Bool
    redex (App (App (App (Atom S) u) v) w)  = True
    redex (App (App (Atom K) u) v)          = True
    redex (App (Atom I) u)                  = True
    redex _                                 = False

    -- |Returns the list of reducible subexpressions, represented as ParseTrees,
    -- of the given ParseTree.
    redexes             :: ParseTree -> [ParseTree]
    redexes t@(App u v)
        | redex t       = t : rest
        | otherwise     = rest
        where rest = (redexes u) ++ (redexes v)
    redexes _           = []

    -- |Finds the reducible subexpressions of the given Expression, if possible.
    tryRedexes          :: Expression -> Maybe [ParseTree]
    tryRedexes e        = tryParse e >>= (\x -> Just $ redexes x)

    tryAnalyze          :: Expression -> Maybe String
    tryAnalyze e        = case tryParse e of
        Just u  ->  Just $ "Expression: " ++ (show e) ++ "\nParse tree: " ++
                    (show u) ++ "\n" ++ (show $ decorate u []) ++
                    "\nRedexes:    " ++ (show $ redexes u)
        Nothing ->  Nothing

    -- |w is the weak reduction function. It implements the following axioms:
    -- 1. S e f g  ->w eg(fg);
    -- 2. K e f    ->w e; and
    -- 3. I e      ->w e
    -- For any other kind of expression, w returns the expression itself. This
    -- function doesn't check recursively for reducible subexpressions.
    w                   :: ParseTree -> ParseTree
    w (App (App (App (Atom S) u) v) w)  = App (App u w) (App v w)
    w (App (App (Atom K) u) v)          = u
    w (App (Atom I) u)                  = u
    w t                                 = t

    -- |Performs a single step of reduction using the w reduction function. The
    -- step is performed using normal evaluation order, which means that the
    -- parent node has higher priority than its child nodes, and the left child
    -- has higher priority than the right child. If the given ParseTree doesn't
    -- contain any reducible subtrees, then this function returns the argument.
    step                :: ParseTree -> ParseTree
    step t@(App u v)
        | redex t       = w t
        | otherwise     =
            let x       = step u
            in  if x /= u then App x v else App u $ step v
    step t              = t

    -- |Performs the given number of steps with the given Expression.
    stepN               :: Int -> ParseTree -> ParseTree
    stepN n
        | n == 0        = id
        | otherwise     = \t ->
            let u       = step t
            in  if u == t then u else stepN (n - 1) u

    -- |Like stepN, but prints the intermediary results. The first component of
    -- the (Int, Int) pair is the number of steps and the second one is a
    -- counter used for printing the ordinals of the steps.
    traceN              :: (Int, Int) -> ParseTree -> ParseTree
    traceN (n,k)
        | n == 0        = \t -> trace ((show k) ++ ":\t" ++ (show t)) t
        | otherwise     = \t -> trace ((show k) ++ ":\t" ++ (show t)) $
            let u       = step t
            in  if u == t then u else traceN (n - 1, k + 1) u

    -- |Performs all the available reduction steps for the given Expression.
    -- This function will not return if the Expression doesn't have a w-normal
    -- form.
    reduce              :: ParseTree -> ParseTree
    reduce e            = let f = step e in if e == f then e else reduce f

    -- |Tries to perform a single step of reduction on the given Expression, if
    -- possible.
    tryStep             :: Expression -> Maybe ParseTree
    tryStep e           = tryParse e >>= (\t -> Just $ step t)

    -- |Tries to perform multiple steps on the given Expression, if possible.
    tryStepN            :: Int -> Expression -> Maybe ParseTree
    tryStepN n e        = tryParse e >>= (\t -> Just $ stepN n t)

    -- |Like tryStepN, but prints the intermediary steps.
    tryTraceN           :: Int -> Expression -> Maybe ParseTree
    tryTraceN n e       = tryParse e >>= (\t -> Just $ traceN (n,0) t)

    -- |Produces a list of ParseTrees of exactly the given length by iterating
    -- step on the given Expression.
    stepNList           :: Int -> ParseTree -> [ParseTree]
    stepNList n t       = take n $ iterate step t

    -- |Applies stepNList with the given length and Expression, if possible.
    tryStepNList        :: Int -> Expression -> Maybe [ParseTree]
    tryStepNList n e    = tryParse e >>= (\t -> Just $ stepNList n t)

    -- |Like tryTraceN, but uses Polish notation for output.
    tryTraceNPolish     :: Int -> Expression -> Maybe Expression
    tryTraceNPolish n e = tryParse e >>= (\t ->
        Just $ fromParseTree $ snd $ last $
        let f n t       = (show n) ++ ":\t" ++ (show $ fromParseTree t)
            g (n,t)     = (n + 1, trace (f n t) $ step t)
        in  takeWhile (\(k,_) -> k - 1 <= n) $ iterate g (0,t))

    -- |Tries to reduce the given Expression, if possible.
    tryReduce           :: Expression -> Maybe ParseTree
    tryReduce e         = tryParse e >>= (\x -> Just $ reduce x)

    -- |This helper function performs the given operation on the given
    -- expression, getting rid of the Maybe monad, and returning a String.
    perform             :: (Show a) => (Expression -> Maybe a) -> Expression -> String
    perform f x         = case f x of
        Just y          -> show y
        Nothing         -> "\"" ++ (show x) ++ "\" is malformed."

    -- |Command is an algebraic type that is mapped to operations of this
    -- interpreter. Each constructor corresponds with an unique operation. The
    -- outcome of this operation is evaluated when executing "show". The
    -- operations fail on malformed Expressions.
    data Command        = -- |Parse tries to convert an Expression into a
                          -- ParseTree
                          Parse Expression
                          -- |Decorate tries to convert an Expression into a
                          -- FancyTree
                        | Decorate Expression
                          -- |FindRedexes tries to find the reducible
                          -- subexpressions of the given Expression.
                        | FindRedexes Expression
                          -- |Analyze is a combination of the commands Parse,
                          -- Decorate, and FindRedexes.
                        | Analyze Expression
                          -- |Step tries to perform a single step of
                          -- w-reduction.
                        | Step Expression
                          -- |StepN tries the given number of reduction steps on
                          -- the given Expression.
                        | StepN Int Expression
                          -- |TraceN is like StepN, but prints intermediary
                          -- steps in stdout.
                        | TraceN Int Expression
                          -- |TraceNPolish is like TraceN, but uses the Polish
                          -- (prefix) notation.
                        | TraceNPolish Int Expression
                          -- |Reduce tries to reduce the given Expression until
                          -- a normal form is obtained or two consecutive steps
                          -- yield identical output.
                        | Reduce Expression

    instance Show Command where
        show c          = case c of
            Parse e             -> perform tryParse e
            Decorate e          -> perform tryDecorate e
            FindRedexes e       -> perform tryRedexes e
            Analyze e           -> perform tryAnalyze e
            Step e              -> perform tryStep e
            StepN n e           -> perform (tryStepN n) e
            TraceN n e          -> perform (tryTraceN n) e
            TraceNPolish n e    -> perform (tryTraceNPolish n) e
            Reduce e            -> perform tryReduce e
--            _               -> "Unsupported command."

    -- |Omega is the reducible expression (SII)(SII), for which holds that:
    -- Omega    === (SII)(SII)
    --          ->w (I(SII))(I(SII))
    --          ->w (SII)(I(SII))
    --          ->w (SII)(SII)
    --          === Omega
    -- Thus, Omega ->w* Omega. Omega is analogous to (\x->xx)(\x->xx) in LC.
    -- However, since this interpreter uses normal reduction order, we get:
    -- Omega    === (SII)(SII)
    --          ->w (I(SII))(I(SII))
    --          ->w (SII)(I(SII))
    --          ->w (I(I(SII)))(I(I(SII)))
    --          ->w (I(SII))(I(I(SII)))
    --          ->w (SII)(I(I(SII)))
    --          ->w (I(I(I(SII))))(I(I(I(SII))))
    --          ...
    omega   :: Expression
    omega   = [A,A,A,S,I,I,A,A,S,I,I]

    -- |Y is a fix point combinator. A fix point combinator is a combinator X
    -- that satisfies X e = e (X e) for any expression e.
    y       :: Expression
    y       = [A,A,S,A,A,S,A,K,A,A,S,S,A,K,A,A,S,I,I,K,A,A,S,A,K,A,A,S,S,A,K,A,A,S,I,I,K]

    -- |Turing's Theta is another fix point combinator.
    theta   :: Expression
    theta   = fromString "***S*K**SS*S**SIIK**S*K**SS*S**SIIK"

    -- |The I combinator is extensionally equivalent to SKK.
    i       :: Expression
    i       = [A,A,S,K,K]

    -- |T combinator can used for repsesenting truth in an if-then else
    -- expression. For example, the C-style ternary operator
    -- <condition> ? <then-branch> : <else-branch> can be encoded simply as
    -- P <then-branch> <else-branch> where P is either T or F.
    t       :: Expression
    t       = [K]
    -- |F combinator can used for repsesenting falsity in an if-then else
    -- expression.
    f       :: Expression
    f       = [A,K,I]
