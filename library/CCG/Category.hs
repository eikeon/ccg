module CCG.Category where

data Category =
      Category :/ Category  -- A / B - waiting argument on right side
    | Category :\ Category  -- A \ B - waiting argument on left side
    | Category String
    | Conjunct Category
    deriving (Eq)

instance Show Category where
    show (l :/ r) = show l ++ "/" ++ show r
    show (l :\ r) = show l ++ "\\" ++ show r
    show (Category _syntax) = _syntax
    show (Conjunct r) = "Conjunct (" ++ show r ++  ")"

type Combinator =  Category -> Category -> Maybe Category  -- result if combinator applicable to arguments

data Rule =
    ForwardApplication |
    BackwardApplication |
    ForwardComposition |
    BackwardComposition |
    ForwardSubstitution |
    BackwardXSubstitution |
    ForwardTypeRaisingComposition |
    BackwardTypeRaisingComposition |
    LeftForwardTypeRaisingComposition |
    LeftBackwardTypeRaisingComposition |
    HalfCoordination |
    Coordination
    deriving (Eq)

instance Show Rule where
    show ForwardApplication = ">"
    show BackwardApplication = "<"
    show ForwardComposition = ">B"
    show BackwardComposition = "<B"
    show ForwardSubstitution = ">S"
    show BackwardXSubstitution = "<Sx"
    show ForwardTypeRaisingComposition = ">T"
    show BackwardTypeRaisingComposition = "<T"
    show LeftForwardTypeRaisingComposition = "<>T"
    show LeftBackwardTypeRaisingComposition = "<<T"
    show HalfCoordination = "~&"
    show Coordination = "&"

class C a where
    combinator :: a -> Combinator

instance C Rule where
    combinator ForwardApplication = forwardApplication
    combinator BackwardApplication = backwardApplication
    combinator ForwardComposition = forwardComposition
    combinator BackwardComposition = backwardComposition
    combinator ForwardSubstitution = forwardSubstitution
    combinator BackwardXSubstitution = backwardXSubstitution
    combinator ForwardTypeRaisingComposition = forwardTypeRaisingComposition
    combinator BackwardTypeRaisingComposition = backwardTypeRaisingComposition
    combinator LeftForwardTypeRaisingComposition = leftForwardTypeRaisingComposition
    combinator LeftBackwardTypeRaisingComposition = leftBackwardTypeRaisingComposition
    combinator HalfCoordination = halfCoordination
    combinator Coordination = coordination


-- X / Y • Y => X (>)
forwardApplication :: Combinator
forwardApplication  (x :/ y)  y'
        | y == y' = Just x
forwardApplication _ _ = Nothing

-- Y • X \ Y => X (<)
backwardApplication :: Combinator
backwardApplication  y  (x :\ y')
        | y == y' = Just x
backwardApplication _ _ = Nothing

-- X / Y • Y / Z => X / Z (> B)
forwardComposition :: Combinator
forwardComposition  (x :/ y)  (y' :/ z)
        | y == y' = Just (x :/ z)
forwardComposition _ _ = Nothing

-- there is also crossing forward and backward composition
-- X / Y • Y \ Z => X \ Z forward
-- X \ Y • Y / Z => X / Z backward

-- X \ Y • Y \ Z => X \ Z (> B)
backwardComposition :: Combinator
backwardComposition  (x :\ y)  (y' :\ z)
        | y == y' = Just (x :\ z)
backwardComposition _ _ = Nothing

-- (X / Y) / Z • Y / Z => X / Z (> S)
forwardSubstitution :: Combinator
forwardSubstitution  ((x :/ y) :/ z)  (y' :/ _)
        | y == y' && y == y' = Just (x :/ z)
forwardSubstitution _ _ = Nothing

-- Y / Z • (X \ Y) / Z => X / Z (< Sx)
backwardXSubstitution :: Combinator
backwardXSubstitution  (y :/ z)  ((x :\ y') :/ _)
        | y == y' && y == y' = Just (x :/ z)
backwardXSubstitution _ _ = Nothing

{-
    Type Raising
    In real parser we can't use this rule.
    So we should use it with any purpose for composition and our combinator will be pair of type raising and composition.
    We should use some euristics to prevent infinite type raising. Many implementations have different simple usages of type raising.
    There will be used NLTP algorithm.
-}

-- X => T / (T \ X) (>T)
forwardTypeRaising :: Category -> Category -> Category
forwardTypeRaising x t = t :/ (t :\ x)

-- X => T \ (T / X) (<T)
backwardTypeRaising :: Category -> Category -> Category
backwardTypeRaising x t = t :\ (t :/ x)


-- very simple usage of type raising
-- X • (Y \ X) / Z => Y / (Y \ X) • (Y \ X) / Z (>T) => Y / Z (>B)
forwardTypeRaisingComposition :: Combinator
forwardTypeRaisingComposition x right@((y :\ x') :/ _)
        | x == x' = forwardComposition (forwardTypeRaising x y) right
forwardTypeRaisingComposition _ _ = Nothing

-- X • (Y / X) \ Z => Y \ (Y / X) • (Y / X) \ Z (<T) => Y \ Z (<B)
backwardTypeRaisingComposition :: Combinator
backwardTypeRaisingComposition x right@((y :/ x') :\ _)
        | x == x' = backwardComposition (backwardTypeRaising x y) right
backwardTypeRaisingComposition _ _ = Nothing

-- (X \ Y) / Z • R => (X \ Y) / Z • Z / (Z \ R) (>T) => (X \ Y) / (Z \ R) (>B)
leftForwardTypeRaisingComposition :: Combinator
leftForwardTypeRaisingComposition left@((_ :\ _) :/ z) r = forwardComposition left (forwardTypeRaising r z)
leftForwardTypeRaisingComposition _ _ = Nothing

-- (X / Y) \ Z • R => (X / Y) \ Z • Z \ (Z / R) (<T) => (X / Y) \ (Z / R) (<B)
leftBackwardTypeRaisingComposition :: Combinator
leftBackwardTypeRaisingComposition left@((_ :/ _) :\ z) r = backwardComposition left (backwardTypeRaising r z)
leftBackwardTypeRaisingComposition _ _ = Nothing

-- exception of combinators
-- X conj X => x (<&>)
-- so we can implement him in two steps

halfCoordination :: Combinator
halfCoordination (Category "C") x = Just $ Conjunct x
halfCoordination _ _ = Nothing

coordination :: Combinator
coordination x (Conjunct x') | x == x' = Just x
coordination _ _ = Nothing
