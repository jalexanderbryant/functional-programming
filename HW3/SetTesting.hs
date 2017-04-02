-- Added 6 tests for powerSet.  Rewrote existing tests to use HUnit. --JDJ-S
module SetTesting where

import System.Random
import Set
import Data.Set (union, difference, intersection, fromList, toList)
import Data.List
import Test.HUnit

make_pset_test input answer = "powerSet " ++ show input ~: powerSet input ~?= answer

simple = [1..3]
simple_pset = [[],[1],[1,2],[1,2,3],[1,3],[2],[2,3],[3]]

chars = "blah"
char_pset = [[],['a'],['a','b'],['a','b','h'],['a','b','h','l'],['a','b','l'],['a','h'],['a','h','l'],['a','l'],['b'],['b','h'],['b','h','l'],['b','l'],['h'],['h','l'],['l']]

pset_tests = TestList [
            make_pset_test (empty :: Set Int) (sing empty),
            make_pset_test (sing 'a') (makeSet [empty, sing 'a']),
            make_pset_test (makeSet simple) (makeSet $ map makeSet simple_pset),
            make_pset_test (makeSet chars) (makeSet $ map makeSet char_pset),
            make_pset_test (makeSet five) (makeSet $ map makeSet five_pset),
            make_pset_test (makeSet ps) (makeSet $ map makeSet pps)
           ]
          
main =
  do
    g <- getStdGen
    let randomList = (randomRs (0::Int, 10) g)
    let (as, randomList') = splitAt 5 randomList
    let (bs, _)           = splitAt 5 randomList'
    let tests = "Problem 2" ~: TestList [
                 "a)" ~: unwords ("diff" : map (show . makeSet) [as,bs])  ~: 
                            diff (makeSet as) (makeSet bs) ~?= makeSet ( toList ( difference (fromList as) (fromList bs) ) ),
                 "b)" ~: unwords ("symDiff" : map (show . makeSet) [as,bs])  ~: 
                               symDiff (makeSet as) (makeSet bs) ~?= makeSet ( toList ( Data.Set.difference ( Data.Set.union (fromList as) (fromList bs) ) ( Data.Set.intersection (fromList as) (fromList bs) ) ) ),
                 "c) powerSet" ~: pset_tests
                ]
    runTestTT tests 


five = ["wood", "fire", "water", "metal", "earth"]

five_pset = [[],["earth"],["earth","fire"],["earth","fire","metal"],["earth","fire","metal","water"],["earth","fire","metal","water","wood"],["earth","fire","metal","wood"],["earth","fire","water"],["earth","fire","water","wood"],["earth","fire","wood"],["earth","metal"],["earth","metal","water"],["earth","metal","water","wood"],["earth","metal","wood"],["earth","water"],["earth","water","wood"],["earth","wood"],["fire"],["fire","metal"],["fire","metal","water"],["fire","metal","water","wood"],["fire","metal","wood"],["fire","water"],["fire","water","wood"],["fire","wood"],["metal"],["metal","water"],["metal","water","wood"],["metal","wood"],["water"],["water","wood"],["wood"]]


ps = ["Twilight Sparkle", "Pinkie Pie", "Applejack", "Rainbow Dash", "Rarity", "Fluttershy"]

pps = [[],["Applejack"],["Applejack","Fluttershy"],["Applejack","Fluttershy","Pinkie Pie"],["Applejack","Fluttershy","Pinkie Pie","Rainbow Dash"],["Applejack","Fluttershy","Pinkie Pie","Rainbow Dash","Rarity"],["Applejack","Fluttershy","Pinkie Pie","Rainbow Dash","Rarity","Twilight Sparkle"],["Applejack","Fluttershy","Pinkie Pie","Rainbow Dash","Twilight Sparkle"],["Applejack","Fluttershy","Pinkie Pie","Rarity"],["Applejack","Fluttershy","Pinkie Pie","Rarity","Twilight Sparkle"],["Applejack","Fluttershy","Pinkie Pie","Twilight Sparkle"],["Applejack","Fluttershy","Rainbow Dash"],["Applejack","Fluttershy","Rainbow Dash","Rarity"],["Applejack","Fluttershy","Rainbow Dash","Rarity","Twilight Sparkle"],["Applejack","Fluttershy","Rainbow Dash","Twilight Sparkle"],["Applejack","Fluttershy","Rarity"],["Applejack","Fluttershy","Rarity","Twilight Sparkle"],["Applejack","Fluttershy","Twilight Sparkle"],["Applejack","Pinkie Pie"],["Applejack","Pinkie Pie","Rainbow Dash"],["Applejack","Pinkie Pie","Rainbow Dash","Rarity"],["Applejack","Pinkie Pie","Rainbow Dash","Rarity","Twilight Sparkle"],["Applejack","Pinkie Pie","Rainbow Dash","Twilight Sparkle"],["Applejack","Pinkie Pie","Rarity"],["Applejack","Pinkie Pie","Rarity","Twilight Sparkle"],["Applejack","Pinkie Pie","Twilight Sparkle"],["Applejack","Rainbow Dash"],["Applejack","Rainbow Dash","Rarity"],["Applejack","Rainbow Dash","Rarity","Twilight Sparkle"],["Applejack","Rainbow Dash","Twilight Sparkle"],["Applejack","Rarity"],["Applejack","Rarity","Twilight Sparkle"],["Applejack","Twilight Sparkle"],["Fluttershy"],["Fluttershy","Pinkie Pie"],["Fluttershy","Pinkie Pie","Rainbow Dash"],["Fluttershy","Pinkie Pie","Rainbow Dash","Rarity"],["Fluttershy","Pinkie Pie","Rainbow Dash","Rarity","Twilight Sparkle"],["Fluttershy","Pinkie Pie","Rainbow Dash","Twilight Sparkle"],["Fluttershy","Pinkie Pie","Rarity"],["Fluttershy","Pinkie Pie","Rarity","Twilight Sparkle"],["Fluttershy","Pinkie Pie","Twilight Sparkle"],["Fluttershy","Rainbow Dash"],["Fluttershy","Rainbow Dash","Rarity"],["Fluttershy","Rainbow Dash","Rarity","Twilight Sparkle"],["Fluttershy","Rainbow Dash","Twilight Sparkle"],["Fluttershy","Rarity"],["Fluttershy","Rarity","Twilight Sparkle"],["Fluttershy","Twilight Sparkle"],["Pinkie Pie"],["Pinkie Pie","Rainbow Dash"],["Pinkie Pie","Rainbow Dash","Rarity"],["Pinkie Pie","Rainbow Dash","Rarity","Twilight Sparkle"],["Pinkie Pie","Rainbow Dash","Twilight Sparkle"],["Pinkie Pie","Rarity"],["Pinkie Pie","Rarity","Twilight Sparkle"],["Pinkie Pie","Twilight Sparkle"],["Rainbow Dash"],["Rainbow Dash","Rarity"],["Rainbow Dash","Rarity","Twilight Sparkle"],["Rainbow Dash","Twilight Sparkle"],["Rarity"],["Rarity","Twilight Sparkle"],["Twilight Sparkle"]]
