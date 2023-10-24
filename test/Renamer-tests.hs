{-# LANGUAGE TypeApplications #-}
module Main where

import Test.QuickCheck
import Lang.Abs
import Renamer.Renamer

main :: IO ()
main = quickCheck (withMaxSuccess 1000 renamingIdentity)

-- Use implication
renamingIdentity :: Program -> Bool
renamingIdentity program =
    case renameGetNames mempty Fresh program of
        Left _ -> True
        Right (program', names) -> case rename names Old program' of
            Left _ -> False
            Right program'' ->
                let l = Nothing <$ program'
                    r = Nothing <$ program''
                in (l :: Program) == (r :: Program)

instance Arbitrary Expr where
    arbitrary = oneof
        [ EPat Nothing . Ident <$> arbitrary
        , ELit Nothing <$> arbitrary
        , EApp Nothing <$> arbitrary <*> arbitrary
        , EAppExplicit Nothing <$> arbitrary <*> arbitrary <*> arbitrary
        , ECase Nothing <$> arbitrary <*> arbitrary
        , ELet Nothing <$> arbitrary <*> arbitrary <*> arbitrary
        , ELam Nothing <$> arbitrary <*> arbitrary
        ]

instance Arbitrary Type where
    arbitrary = frequency
        [
        (60, TVar Nothing . MkTVar Nothing <$> arbitrary),
        (20, TFun Nothing <$> arbitrary <*> arbitrary),
        (20, TApp Nothing <$> arbitrary <*> arbitrary)
        ]

instance Arbitrary Branch where
    arbitrary = Branch Nothing <$> arbitrary <*> arbitrary

instance Arbitrary Pattern where
    arbitrary = patterns startPF

data PatternFrequency
    = PF { plit :: Int , pcatch :: Int , pvar :: Int , pinj :: Int }

stepPF :: PatternFrequency -> PatternFrequency
stepPF (PF a b c d) = PF a b c (d - 10)

startPF = PF 20 20 20 40

patterns :: PatternFrequency -> Gen Pattern
patterns pf = frequency [
        (plit pf, PLit Nothing <$> arbitrary),
        (pcatch pf , pure (PCatch Nothing)),
        (pvar pf , PVar Nothing <$> arbitrary),
        (pinj pf , PInj Nothing <$> arbitrary <*> (resize 10 . listOf . patterns . stepPF) pf)
        ]

instance Arbitrary Bind where
    arbitrary = Bind Nothing <$> arbitrary @Ident <*> arbitrary @Expr

instance Arbitrary Ident where
    arbitrary = Ident <$> arbitrary

instance Arbitrary Def where
    arbitrary = DBind Nothing <$> arbitrary @Bind

instance Arbitrary Program where
    arbitrary = Program Nothing <$> arbitrary

instance Arbitrary Lit where
   arbitrary = oneof [ LInt Nothing <$> arbitrary
                     , LChar Nothing <$> arbitrary
                     , LString Nothing <$> arbitrary
                     ]
