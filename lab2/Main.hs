module Main where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe
import Data.List
import Lex
import Synt

type MPMap = Map.Map Exp [Exp]
type ProofMap = Map.Map Exp Exp


checkAxioms :: Exp -> Maybe Int
checkAxioms expr = case expr of
                    (Impl a (Impl b c))
                        | a == c -> Just 1
                    (Impl (Impl a b) (Impl (Impl c (Impl d e)) (Impl f g)))
                        | a == c && a == f && b == d && e == g -> Just 2
                    (Impl a (Impl b (And c d)))
                        | a == c && b == d -> Just 3
                    (Impl (And a b) c)
                        | a == c -> Just 4
                        | b == c -> Just 5
                    (Impl a (Or b c))
                        | a == b -> Just 6
                        | a == c -> Just 7
                    (Impl (Impl a b) (Impl (Impl c d) (Impl (Or e f) g)))
                        | a == e && c == f && b == d && b == g -> Just 8
                    (Impl (Impl a b) (Impl (Impl c (Not d)) (Not e)))
                        | a == c && a == e && b == d -> Just 9
                    (Impl (Not(Not a)) b)
                        | a == b -> Just 10
                    (a) -> Nothing

addModusPonens :: Exp -> MPMap -> MPMap
addModusPonens expr mp = case expr of
                            (Impl a b) -> case Map.lookup b mp of
                                                Nothing -> Map.insert b [a] mp
                                                Just list -> Map.insert b (list ++ [a]) mp
                            (a) -> mp

addProof :: Exp -> Set.Set Exp -> Set.Set Exp
addProof expr proof = Set.insert expr proof

checkProof :: [Exp] -> Set.Set Exp -> Maybe Exp
checkProof [] _ = Nothing
checkProof (expr:exprs) proof = case Set.member expr proof of
                                False -> checkProof exprs proof
                                True -> Just expr

checkModusPonens :: Exp -> MPMap -> Set.Set Exp -> Maybe Exp
checkModusPonens expr mp proof = let
                                      list = Map.lookup expr mp in
                                    if isJust $ list
                                        then checkProof (fromJust list) proof
                                    else Nothing


checkHyp :: [Exp] -> Exp -> Maybe Int
checkHyp hyp exp = elemIndex exp hyp




{- if exp is MP then (0 exp) if exp is axiom then (num exp) if exp is hyp then ((10 + num) exp) -}
{- исх строчки, уже встречавшиеся, из b в [a], гипотезы, входные строчки, из b в a -}
parseProof :: [Exp] -> Set.Set Exp -> MPMap -> [Exp] -> ProofMap-> [(Int, Exp)] ->(Set.Set Exp, MPMap, Bool, ProofMap, [(Int, Exp)])
parseProof [] proofs mp hyp map all = (proofs, mp, True, map, all)
parseProof (exp:lines) proofs mp hyp map all = let

                                        isAxiom = checkAxioms exp
                                        isHyp = checkHyp hyp exp
                                        isMP = checkModusPonens exp mp proofs in
                                      if Set.member exp proofs
                                        then parseProof lines proofs mp hyp map all
                                      else if isJust isAxiom
                                        then parseProof lines (addProof exp proofs) (addModusPonens exp mp) hyp map (((fromJust isAxiom), exp ) : all)
                                      else if isJust isHyp
                                        then parseProof lines (addProof exp proofs) (addModusPonens exp mp) hyp map (((fromJust isHyp + 11), exp ) : all)
                                      else if isJust isMP {- здесь фиксируем конкретный МП -}
                                        then parseProof lines  (addProof exp proofs) (addModusPonens exp mp) hyp (Map.insert exp (fromJust isMP) map) ((0, exp) : all)
                                      else (proofs, mp, False, map, all)

dlt :: [(Int, Exp)] -> ProofMap -> Exp -> Set.Set (Exp) -> Set.Set (Exp)
dlt [] map exx result = result
dlt ((a, exp):pairs) map exx result =
                                        if exp /= exx
                                          then dlt pairs map exx result
                                        else fmp ((a, exp):pairs) map exx result

fmp :: [(Int, Exp)] -> ProofMap -> Exp -> Set.Set (Exp) -> Set.Set (Exp)
fmp ((num1, exp):pairs) map b result =
                                if exp /= b
                                  then fmp pairs map b result
                                else if 0 < num1
                                  then (Set.insert b result)
                                else let
                                    a = fromJust (Map.lookup b map)
                                    aimplb = Impl a b in
                                fmp pairs map a (fmp pairs map aimplb (Set.insert b result))




printList :: [Exp] -> IO()
printList [] = return ()
printList (expr:exprs) = do
                            putStr $ show expr
                            if (length exprs) > 0
                                then putStr $ ", "
                            else putStr $ " "
                            printList exprs

printDoc :: [(Int, Exp)] ->  Int -> Set.Set (Exp) -> ProofMap -> [(Int, Exp, Int, Int, Int)] ->[(Int, Exp, Int, Int, Int)]
printDoc [] _ _ _ printed = printed
printDoc ((expnum, exp):pairs) num used map printed =
                                            if not (Set.member exp used)
                                              then printDoc pairs num used map  printed
                                            else if expnum > 0
                                              then if expnum < 11
                                                then printDoc pairs (num + 1) used map ((num, exp, 1, expnum, 0 ) : printed)
                                              else printDoc pairs (num + 1) used map ((num, exp, 2, (expnum - 10), 0) : printed)
                                            else let
                                             a = fromJust (Map.lookup exp map)
                                             aimplb = Impl a exp in
                                            printDoc pairs (num + 1) used map ((num, exp, 3, (findNum printed aimplb), (findNum printed a)) : printed)


findNum :: [(Int, Exp, Int, Int, Int)] -> Exp -> Int
findNum [] _ = 0
findNum ((num, exp, _, _, _): pairs) exx =
                                    if exp /= exx
                                      then findNum pairs exx
                                    else num

printDocc :: [(Int, Exp, Int, Int, Int)] -> IO()
printDocc [] = return ()
printDocc ((num, exp, tp, a, b):strings) = do
                            if tp == 1
                              then putStrLn ("[" ++ (show num) ++ ". Ax. sch. " ++ (show a) ++ "] " ++ (show exp))
                            else if tp == 2
                              then putStrLn ("[" ++ (show num) ++ ". Hypothesis " ++ (show a) ++ "] " ++ (show exp))
                            else putStrLn ("[" ++ (show num) ++ ". M.P. " ++ (show a) ++ ", " ++ (show b) ++ "] " ++ (show exp))
                            printDocc strings

parseExp ::[String] -> [Exp]-> [Exp]
parseExp [] exps = exps
parseExp (line : lines) exps = parseExp lines ((parse $ alexScanTokens line) : exps)

main :: IO()
main = do
        contents <- getContents
        let strings = lines $ contents
        let fstline = parseFirstLine $ alexScanTokens (head strings)
        let exx = head fstline
        let hyp = reverse (tail fstline)
        let exps = parseExp (tail strings) []
        let (proofs, _, bol, map, all) = parseProof (reverse exps) Set.empty Map.empty hyp Map.empty []
        if (head exps) /= exx|| not bol || not (Set.member exx proofs)
          then putStr $ "Proof is incorrect"
        else do
                  let used = dlt all map exx Set.empty
                  let toPrint = printDoc (reverse all)  1 used map []
                  printList hyp
                  putStrLn $ "|- " ++ show exx
                  printDocc $ reverse toPrint
