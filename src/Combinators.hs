module Combinators (fromString) where

    import CombinatoryCalculator

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
