{-# LANGUAGE TypeApplications #-}
module Main where

import Test.QuickCheck
import Lang.Abs
import Renamer.Renamer

main :: IO ()
main = quickCheck renamingIdentity

-- Use implication
renamingIdentity :: Program -> Property
renamingIdentity program =
    case renameGetNames mempty Fresh program of
        Left _ -> True === True
        Right (names, program') -> case rename names Old program' of
            Left _ -> True === False
            Right program'' -> isEqual @Program (Nothing <$ program') (Nothing <$ program'')

isEqual :: (Show a, Eq a) => a -> a -> Property
isEqual = (===)

instance Arbitrary Expr where
    arbitrary = EPat Nothing . Ident <$> arbitrary

instance Arbitrary Bind where
    arbitrary = Bind Nothing <$> arbitrary @Ident <*> arbitrary @Expr

instance Arbitrary Ident where
    arbitrary = Ident <$> arbitrary

instance Arbitrary Def where
    arbitrary = DBind Nothing <$> arbitrary @Bind

instance Arbitrary Program where
    arbitrary = Program Nothing <$> arbitrary
