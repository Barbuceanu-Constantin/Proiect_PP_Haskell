-- =============== DO NOT MODIFY ===================

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

-- ==================================================

module Tasks where

import Dataset ()
import Data.List ( intersperse, sortBy )
import Text.Printf ( printf )
import Data.Array ()

type CSV = String
type Value = String
type Row = [Value]
type Table = [Row]


-- Prerequisities
split_by :: Char -> String -> [String]
split_by x = foldr op [""]
  where op char acc
            | char == x = "":acc
            | otherwise = (char:head(acc)):tail(acc)

read_csv :: CSV -> Table
read_csv = (map (split_by ',')) . (split_by '\n')

write_csv :: Table -> CSV
write_csv = (foldr (++) []).
            (intersperse "\n").
            (map (foldr (++) [])).
            (map (intersperse ","))


{-
    TASK SET 1
-}
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
--Task 1
{-Table = [Row] = [[Value]] = [[String]]-}

table_header1 :: [[[Char]]]
table_header1 = [["Name", "Average Number of Steps"]]

read1 :: String -> Float
read1 string = read string :: Float

floatSum :: [String] -> Float
floatSum list_of_numbers = foldl op 0 list_of_numbers
                                            where op acc number = acc + read1 number

--Calculeaza media numarului de pasi pe 8 zile pentru o persoana
calculateAverage2 :: (Integral a) => Float -> a -> String
calculateAverage2 sum nr = (printf "%.2f" (sum / (fromIntegral nr)))

--Returneaza pe rand, apelata fiind de foldl, fiecare linie din tabelul rezultat.
calculateAverage1 :: [String] -> [String]
calculateAverage1 entry = [head entry] ++ [calculateAverage2 (floatSum (tail entry)) (length entry - 1)]

compute_average_steps :: Table -> Table
compute_average_steps m = foldl op table_header1 (tail m)
                                  where op acc entry = acc ++ [calculateAverage1 entry]
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
-- Task 2
{-Table = [Row] = [[Value]] = [[String]]-}

read2 :: String -> Integer
read2 string = read string :: Integer

-- Number of people who have achieved their goal:
thousandSteps :: [String] -> Bool
thousandSteps entry = (sum . map (read2) $ tail entry) >= 1000

get_passed_people_num :: Table -> Int
get_passed_people_num m = foldl op 0 (tail m)
                                where op acc entry = if thousandSteps entry == True then acc + 1 else acc

-- Percentage of people who have achieved their:
get_passed_people_percentage :: Table -> Float
get_passed_people_percentage m = (fromIntegral $ get_passed_people_num m) / (fromIntegral $ length $ tail m)

-- Average number of daily steps
averageNrDailySteps :: [String] -> Float
averageNrDailySteps entry = (sum . map read1 $ tail entry)

get_steps_avg :: Table -> Float
get_steps_avg m = (foldl op 0 $ tail m) / (fromIntegral $ length $ tail m)
                          where op acc entry = acc + averageNrDailySteps entry
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
-- Task 3
-- Practic asta este relatia de baza pe care se bazeaza "foldl_With_2_Accumulators" cand creeaza rezultatul.
-- [map (!!1) (tail m)] ++ [map (!!2) (tail m)] ++ [map (!!3) (tail m)] ++ _ _ _ ++ [map (!!8) (tail m)]

table_header2 :: [[[Char]]]
table_header2 = [["H10","H11","H12","H13","H14","H15","H16","H17"]]

-- Creeaza in acumulator o lista in care fiecare element este o coloana din cele 8
-- pe care se vrea sa se faca media.
foldl_With_2_Accumulators :: Table -> Int -> Table -> Table
foldl_With_2_Accumulators table 9 list_acc = list_acc
foldl_With_2_Accumulators table index_acc list_acc = foldl_With_2_Accumulators table (index_acc + 1) (list_acc ++ [map (!!index_acc) (tail table)])

getSumsPerDays :: Table -> [Float]
getSumsPerDays table = map sum $ map (map read1) (foldl_With_2_Accumulators table 1 [])

get_avg_steps_per_h :: Table -> Table
get_avg_steps_per_h table = table_header2 ++ [map (printf "%.2f") $ map (/fromIntegral (length (tail table))) (getSumsPerDays table)]
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
-- Task 4
table_header3 :: [[Char]]
table_header3 = ["column","range1","range2","range3"]

-- Astea 3 sunt niste predicate folosite de un "filter" pentru a numara cate persoane
-- se incadreaza in intervalele date.
range1 :: Integer -> Bool
range1 nr = nr < 50 && nr >= 0

range2 :: Integer -> Bool
range2 nr = nr >= 50 && nr < 100

range3 :: Integer -> Bool
range3 nr = nr >= 100

-- Numara cate persoane se afla in cele 3 intervale.
enhanced_length :: Table -> Int -> (Integer -> Bool) -> String
enhanced_length m index predicate = show $ length $ filter predicate $ map read2 $ map (!!index) (tail m)

-- Fiecare din astea 3 creeaza linia corespunzatoare din tabelul de output toate 3 fiind concatenate.
veryActive :: Table -> [String]
veryActive m = ["VeryActiveMinutes"] ++ map (enhanced_length m 3) [range1, range2, range3]

fairlyActive :: Table -> [String]
fairlyActive m = ["FairlyActiveMinutes"] ++ map (enhanced_length m 4) [range1, range2, range3]

lightlyActive :: Table -> [String]
lightlyActive m = ["LightlyActiveMinutes"] ++ map (enhanced_length m 5) [range1, range2, range3]

get_activ_summary :: Table -> Table
get_activ_summary m = [table_header3] ++ [veryActive m] ++ [fairlyActive m] ++ [lightlyActive m]
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
-- Task 5
-- Ord : {LT, GT, EQ}
-- Am pus "Total Steps" pentru ca daca luam stringul direct din tabel era "TotalSteps"

select_fst_sec_col :: [String] -> [String]
select_fst_sec_col (name : totalsteps : ls) = [name] ++ [totalsteps]

columns :: Table -> Table
columns m = map select_fst_sec_col $ tail m

-- Functia principala
get_ranking :: Table -> Table
get_ranking m = [[(head m) !! 0] ++ ["Total Steps"]] ++ (sortBy compareOnTotalSteps $ columns m)

-- Comparator1
compareOnNames :: Row -> Row -> Ordering
compareOnNames row1 row2
                      |(row1 !! 0) < (row2 !! 0)  = LT
                      |(row1 !! 0) == (row2 !! 0) = EQ
                      |(row1 !! 0) > (row2 !! 0)  = GT

-- Comparator2
compareOnTotalSteps :: Row -> Row -> Ordering
compareOnTotalSteps row1 row2
                              |read2 (row1 !! 1) < read2 (row2 !! 1) = LT
                              |read2 (row1 !! 1) == read2 (row2 !! 1) = compareOnNames row1 row2
                              |read2 (row1 !! 1) > read2 (row2 !! 1) = GT
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
-- Task 6
table_header4 :: [[[Char]]]
table_header4 = [["Name","Average first 4h","Average last 4h","Difference"]]

-- Media primelor 4 valori
averageFirst4 :: [String] -> String
averageFirst4 entry = printf "%.2f" $ ((read1 (entry !! 1)) + (read1 (entry !! 2)) + (read1 (entry !! 3)) + (read1 (entry !! 4))) / (fromIntegral 4)

-- Media ultimelor 4 valoi
averageLast4 :: [String] -> String
averageLast4 entry = printf "%.2f"  $ ((read1 (entry !! 5)) + (read1 (entry !! 6)) + (read1 (entry !! 7)) + (read1 (entry !! 8))) / (fromIntegral 4)

-- Transforma o intrare din tabelul initial intr-o intrare din tabelul final
convertEntry :: [String] -> [String]
convertEntry entry = [entry !! 0] ++ [firstAverage] ++ [lastAverage] ++ [printf "%.2f" difference]
                                  where firstAverage  = averageFirst4 entry
                                        lastAverage   = averageLast4 entry
                                        difference    = abs ((read1 firstAverage) - (read1 lastAverage))
-- Comparator cerut care se bazeaza pe un comparator deja dat.
compareOnDifference :: Row -> Row -> Ordering
compareOnDifference row1 row2
                              |read1 (row1 !! 3) <  read1 (row2 !! 3) = LT
                              |read1 (row1 !! 3) == read1 (row1 !! 3) = compareOnNames row1 row2
                              |read1 (row1 !! 3) > read1 (row1 !! 3)  = GT

-- Sorteaza liniile dupa criteriul cerut.
computeEntries :: Table -> Table
computeEntries m = sortBy compareOnDifference $ map (convertEntry) m

createDesiredTable :: Table -> Table
createDesiredTable m = table_header4 ++ (computeEntries (tail m))

get_steps_diff_table :: Table -> Table
get_steps_diff_table m = createDesiredTable m
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
-- Task 7
-- Applies the given function to all the values
-- Value = String; Table = [[Value]]

f1 :: Value -> Value
f1 _ = "1"

-- EASY. Am facut functia f1 ca sa ma testez eu manual local.
vmap :: (Value -> Value) -> Table -> Table
vmap f m = map (map f) m
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
-- Task 8
-- Applies the given function to all the entries
-- Table = [Row] ; Row = [String]

-- Am facut functia f2 ca sa testez eu manual functia rmap.
f2 :: [String] -> [String]
f2 _ = ["Paradigme", "de", "programare"]

rmap :: (Row -> Row) -> [String] -> Table -> Table
rmap f s m = [s] ++ map f (tail m)

-- Destul de usor de inteles ce fac astea 2 functii.
totalOfMinutes :: [String] -> Float
totalOfMinutes list = fromIntegral $ sum $ map read2 list

totalNrOfMinutesSlept :: [String] -> String
totalNrOfMinutesSlept listOfMinutes = printf "%.2f" (totalOfMinutes listOfMinutes)

--exemplu: ["Mason.Zoe@stud.cs.pub.ro","327","384","412","340","700","304","360"]
get_sleep_total :: Row -> Row
get_sleep_total r =  [head r] ++ [totalNrOfMinutesSlept (tail r)]

----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
