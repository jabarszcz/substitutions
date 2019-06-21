module Substitution where

import Control.Applicative
import Data.Maybe

import System.Random
import Control.Monad.Random
import Control.Monad.Extra

lambda_prec = 10
app_prec = 9
subs_prec = 11
merge_prec = 8
cons_prec = 7

data Term
  = Lambda Term
  | App Term Term
  | Ref Int
  | Subst Term Substitution

data Substitution
  = Lift Int
  | Cons Term Substitution
  | Merge Substitution Substitution

instance Show Term where
  showsPrec c (Lambda t) =
    showParen (c > lambda_prec) $
    showString "λ" .
    showsPrec lambda_prec t
  showsPrec c (App t1 t2) =
    showParen (c > app_prec) $
    showsPrec app_prec t1 .
    showsPrec (app_prec+1) t2
  showsPrec _ (Ref i) =
    showString ("#" <> show i)
  showsPrec c (Subst t s) =
    showParen (c > subs_prec) $
    showsPrec subs_prec t .
    showString "[" .
    shows s .
    showString "]"

instance Show Substitution where
  showsPrec _ (Lift 0) =
    showString "ε"
  showsPrec _ (Lift i) =
    showString $ "↑" <> show i
  showsPrec c (Cons t s) =
    showParen (c > cons_prec) $
    showString (show t <> " · ") .
    showsPrec cons_prec s
  showsPrec c (Merge s1 s2) =
    showParen (c > merge_prec) $
    showsPrec (merge_prec+1) s1 .
    showString (" • " <> show s2)

merge :: Substitution -> Substitution -> Substitution
merge (Lift 0) s = s
merge (Lift i) (Cons _ s) = Merge (Lift (i-1)) s
merge (Lift l1) (Lift l2) = Lift (l1 + l2)
merge (Lift i) (Merge s1 s2) = Merge (Merge (Lift i) s1) s2
merge (Cons t1 s1) s2 = Cons (Subst t1 s2) (Merge s1 s2)
merge (Merge s1 s2) s3 = Merge s1 (Merge s2 s3)

merge' (Merge s1 s2) = pure $ merge s1 s2
merge' _ = empty

makeSubst :: Term -> Term
makeSubst t@(Subst _ _) = t
makeSubst t = Subst t (Lift 0)

beta (App (Lambda t1) t2) =
  pure $ Subst t1 (Cons t2 (Lift 0))
beta _ =
  empty

beta' (App t1 t2)
  | Subst (Lambda t) s <- makeSubst t1 =
      pure $ Subst t (Cons t2 s)
beta' _ =
  empty

push (Subst (Lambda t) s) =
  pure $ Lambda (Subst t (Cons (Ref 0) (Merge s (Lift 1))))
push (Subst (App t1 t2) s) =
  pure $ App (Subst t1 s) (Subst t2 s)
push (Subst (Ref 0) (Cons t _)) =
  pure t
push (Subst (Ref i) (Cons _ s)) =
  pure $ Subst (Ref (i - 1)) s
push (Subst (Ref i) (Lift j)) =
  pure $ Ref (i + j)
push (Subst (Subst t s1) s2) =
  pure $ Subst t (Merge s1 s2)
push _ =
  empty

recurTerm ::
  (Alternative f) =>
  (Term -> f Term) ->
  (Substitution -> f Substitution) ->
  Term -> f Term
recurTerm ft fs l@(Lambda t) =
  ft l <|> Lambda <$> recurTerm ft fs t
recurTerm ft fs a@(App t1 t2) =
  ft a
  <|> (liftA2 App) (recurTerm ft fs t1) (pure t2)
  <|> (liftA2 App) (pure t1) (recurTerm ft fs t2)
recurTerm ft _ r@(Ref _) =
  ft r
recurTerm ft fs s@(Subst t s') =
  ft s
  <|> (liftA2 Subst) (recurTerm ft fs t) (pure s')
  <|> (liftA2 Subst) (pure t) (recurSubst ft fs s')

recurSubst ::
  (Alternative f) =>
  (Term -> f Term) ->
  (Substitution -> f Substitution) ->
  Substitution -> f Substitution
recurSubst _ fs l@(Lift _) = fs l
recurSubst ft fs c@(Cons t s) =
  fs c
  <|> (liftA2 Cons) (recurTerm ft fs t) (pure s)
  <|> (liftA2 Cons) (pure t) (recurSubst ft fs s)
recurSubst ft fs m@(Merge s1 s2) =
  fs m
  <|> (liftA2 Merge) (recurSubst ft fs s1) (pure s2)
  <|> (liftA2 Merge) (pure s1) (recurSubst ft fs s2)

(<!>) :: (Alternative f) => (a -> f b) -> (a -> f b) -> a -> f b
f <!> g = \a -> f a <|> g a

step :: (Alternative f) => Term -> f Term
step =
  recurTerm (beta' <!> push) merge'

evalLeft :: Term -> [Term]
evalLeft = catMaybes . takeWhile isJust . iterate (>>= step) . pure

oneof :: (RandomGen g) => [a] -> Rand g (Maybe a)
oneof [] = return Nothing
oneof ls = Just . (ls !!) <$> getRandomR (0, (length ls) - 1)

evalRandom :: (RandomGen g) => Term -> Rand g [Term]
evalRandom = iterateMaybeM (oneof . step)

evalAll :: Term -> [[Term]]
evalAll = altUnfold (toMay . step)
  where
    toMay [] = Nothing
    toMay ls = Just ls

altUnfold :: (Alternative m, Monad m) => (a -> Maybe (m a)) -> a -> m [a]
altUnfold f a =
  (a:) <$> maybe (pure []) (>>= altUnfold f) (f a)

printEvalRandom :: Term -> IO ()
printEvalRandom = mapM_ print <=< evalRandIO . evalRandom
