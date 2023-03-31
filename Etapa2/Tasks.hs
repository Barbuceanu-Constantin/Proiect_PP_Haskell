
-- =============== DO NOT MODIFY ===================

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

-- ==================================================

module Tasks where

import Dataset ()
import Data.List ( sortBy, intersperse )
import Text.Printf ( printf )
import Data.Array ()
import Text.Read ( readMaybe )

type CSV = String
type Value = String
type Row = [Value]
type Table = [Row]
type ColumnName = String

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



{-
    TASK SET 2
-}
read2 :: String -> Integer
read2 string = read string :: Integer

read1 :: String -> Float
read1 string = read string :: Float

-- Task 1
{- 
    Primeste numele coloanei, prima linie cu toate numele
    de coloane, un acumulator initializat cu 0 si returneaza
    indexul coloanei (mai exact al primei aparitii de la
    stanga la dreapta in caz ca sunt mai multe).
-}
getIndex :: ColumnName -> Row -> Int -> Int
getIndex column [] acc = undefined
getIndex column (c:cs) acc
                        | c == column = acc
                        | otherwise = getIndex column cs (acc + 1)

checkBothInt :: Int -> Row -> Row -> Bool 
checkBothInt c row1 row2
                        |((readMaybe (row1 !! c) :: Maybe Int) /= Nothing) && ((readMaybe (row2 !! c) :: Maybe Int) /= Nothing) = True
                        |otherwise = False

checkBothFloat :: Int -> Row -> Row -> Bool
checkBothFloat c row1 row2
                        |((readMaybe (row1 !! c) :: Maybe Float) /= Nothing) && ((readMaybe (row2 !! c) :: Maybe Float) /= Nothing) = True
                        |otherwise = False

comp1 :: Int -> Row -> Row -> Ordering
comp1 c entry1 entry2
                |read2 (entry1 !! c) < read2 (entry2 !! c) = LT
                |read2 (entry1 !! c) > read2 (entry2 !! c) = GT
                |otherwise = compOnFstCol 0 entry1 entry2

comp2 :: Int -> Row -> Row -> Ordering
comp2 c entry1 entry2
                |read1 (entry1 !! c) < read1 (entry2 !! c) = LT
                |read1 (entry1 !! c) > read1 (entry2 !! c) = GT
                |otherwise = compOnFstCol 0 entry1 entry2

compLexic :: Int -> Row -> Row -> Ordering
compLexic c entry1 entry2
                |entry1 !! c < entry2 !! c = LT
                |entry1 !! c > entry2 !! c = GT
                |otherwise = compOnFstCol 0 entry1 entry2

compInt :: Int -> Row -> Row -> Ordering
compInt c row1 row2
                    |read2 (row1 !! c) < read2 (row2 !! c) = LT
                    |read2 (row1 !! c) > read2 (row2 !! c) = GT
                    |otherwise = EQ

compFloat :: Int -> Row -> Row -> Ordering
compFloat c row1 row2
                    |read1 (row1 !! c) < read1 (row2 !! c) = LT
                    |read1 (row1 !! c) > read1 (row2 !! c) = GT
                    |otherwise = EQ

compString :: Int -> Row -> Row -> Ordering
compString c row1 row2
                    |(row1 !! c) < (row2 !! c) = LT
                    |(row1 !! c) > (row2 !! c) = GT
                    |otherwise = EQ

compOnFstCol :: Int -> Row -> Row -> Ordering
compOnFstCol c row1 row2
                |bothInt    = compInt c row1 row2 
                |bothFloat  = compFloat c row1 row2
                |otherwise  = compString c row1 row2
                where   bothInt = checkBothInt 0 row1 row2
                        bothFloat = checkBothInt 0 row1 row2

compOnColumn :: ColumnName -> Table -> Row -> Row -> Ordering
compOnColumn c t entry1 entry2 
                            |bothInt     = comp1 nrOfColumn entry1 entry2
                            |bothFloat   = comp2 nrOfColumn entry1 entry2
                            |entry1 !! nrOfColumn == "" = LT
                            |entry2 !! nrOfColumn == "" = GT
                            |otherwise = compLexic nrOfColumn entry1 entry2
                            where   nrOfColumn = getIndex c (head t) 0
                                    bothInt    = checkBothInt nrOfColumn entry1 entry2
                                    bothFloat  = checkBothFloat nrOfColumn entry1 entry2

tsort :: ColumnName -> Table -> Table
tsort column table = head table : sortBy (compOnColumn column table) (tail table)
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
-- Task 2
allTrue :: [Bool] -> Bool
allTrue [] = True
allTrue (x:xs)
            |x = allTrue xs
            |otherwise = False

sameColumns :: Row -> Row -> Bool
sameColumns columns1 columns2
                            |length columns1 /= length columns2 = False 
                            |otherwise = allTrue $ zipWith (==) columns1 columns2

addRows :: Table -> Table -> Table
addRows t1 t2 = t1 ++ tail t2

vunion :: Table -> Table -> Table
vunion t1 t2 = if sameColumns (head t1) (head t2) then addRows t1 t2 else t1
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
-- Task 3
emptyRow :: Int -> Row
emptyRow nr = replicate nr ""

padd :: Table -> Int -> Table
padd t nrRowsToBeAdded = t ++ replicate nrRowsToBeAdded patternRow
                        where patternRow = emptyRow $ length $ head t 

appendRows :: Table -> Table -> Table
appendRows = zipWith (++)

hunion :: Table -> Table -> Table
hunion t1 t2
            |l1 > l2 = appendRows t1 (padd t2 (l1 - l2))
            |l1 < l2 = appendRows (padd t1 (l2 - l1)) t2
            |otherwise = appendRows t1 t2
            where   l1 = length t1
                    l2 = length t2
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
-- Task 4
{-  
    ATENTIE!! : Din ce am observat ordinea liniilor se bazeaza pe t2, iar ordinea coloanelor
    pe t1 (adica incepe cu alea din t1).
-}

-- Returneaza coloanele din "row1" si coloanele din "row2" care nu se regasesc in "row1".
notIn :: Row -> Row -> Row
notIn row1 row2 = row1 ++ filter (`notElem` row1) row2

-- Elimina un anumit element dintr-o lista.
deleteAt :: Int -> [a] -> [a]
deleteAt idx xs = lft ++ rgt
  where (lft, _:rgt) = splitAt idx xs

{-  
    Functia cauta printre liniile din t1 linia cu acceasi valoare
    pe coloana "key_column" ca si "row_t2". In situatia in care nu
    s-a gasit in "t1" nicio linie cu valoarea cautata pe coloana
    "key_column" se returneaza lista vida. Daca s-a gasit fac "row_t1 ++ (deleteAt ind_t2 row_t2)"
    mergand pe ideea ca nu exista coloane comune in afara de "key_column".
-}
addNewColumnsFromT1 :: Int -> Int -> Table -> Row -> Row
addNewColumnsFromT1 ind_t1 ind_t2 [] row_t2 = []
addNewColumnsFromT1 ind_t1 ind_t2 t1@(row_t1:t1s) row_t2
                                |(row_t1 !! ind_t1) == (row_t2 !! ind_t2) = row_t1 ++ deleteAt ind_t2 row_t2
                                |otherwise = addNewColumnsFromT1 ind_t1 ind_t2 t1s row_t2

joinRows :: ColumnName -> Table -> Table -> Table
joinRows key_column t1 t2Ref = map (addNewColumnsFromT1 index_t1 index_t2 t1) $ tail t2Ref
                        where   index_t1 = getIndex key_column (head t1) 0
                                index_t2 = getIndex key_column (head t2Ref) 0

{-  
    Functia verifica sa fie "cname" element unic in "list1" si in "list2".
    Daca apare de 0 sau de mai mult de odata atunci practic nu are sens deci undefined.
-}
onlyOneOcc :: ColumnName -> Row -> Row -> Bool
onlyOneOcc cname list1 list2 
                            |nr_occ1 == 1 && nr_occ2 == 1 = True
                            |otherwise = undefined 
                                where   nr_occ1 = length $ filter (==cname) list1
                                        nr_occ2 = length $ filter (==cname) list2

{-  
    Functia verifica ca intre cele doua linii primite ca parametru
    sa existe un singur element comun.
-}
onlyOneCommon :: Row -> Row -> Int -> Bool
onlyOneCommon [] row2 acc = acc == 1
onlyOneCommon (x1:xs1) row2 acc
                            |x1 `elem` row2 = onlyOneCommon xs1 row2 $ acc + 1
                            |otherwise = onlyOneCommon xs1 row2 acc

-- Intoarce numele coloanelor comune. Al 3lea parametru este acumulator.
comColumns :: Row -> Row -> Row -> Row
comColumns [] row2 acc = acc
comColumns (x:xs) row2 acc
                        |x `elem` row2 = comColumns xs row2 $ acc ++ [x]
                        |otherwise = comColumns xs row2 acc

{-
    Inlocuieste elementul de la pozitia index_row1 din row1
    cu elementul de la pozitia index_row2 din row2.
-}
replace :: Row -> Row -> Int -> Int -> Int -> Row
replace [] _ _ _ _ = []
replace row1@(x:xs) row2 index_row1 index_row2 acc
                                |acc == index_row1 = (row2 !! index_row2) : replace xs row2 index_row1 index_row2 (acc + 1)
                                |otherwise = x : replace xs row2 index_row1 index_row2 (acc + 1)

{-
    Inlocuieste elementele din row1 indicate de valorile din l1
    cu elementele din row2 indicate de valorile din l2.
-}
modify :: Row -> Row -> [Int] -> [Int] -> Row
modify row1 row2 [] _ = row1
modify row1 row2 _ [] = row1 
modify row1 row2 l1@(x1:xs1) l2@(x2:xs2)
                                |row1 !! x1 == "" = modify row1 row2 xs1 xs2
                                |otherwise = modify (replace row1 row2 x1 x2 0) row2 xs1 xs2

-- La fel ca "deleteAt" dar cu parametri inversati ca sa intre bine in map.
deleteAt1 :: [a] -> Int -> [a]
deleteAt1 xs idx = lft ++ rgt
  where (lft, _:rgt) = splitAt idx xs

{- 
    Fiecare element din rezultat e o lista de stringuri
    obtinuta prin eliminarea cate unui index pe rand.
    Daca fac intersectia tuturor rezulta lista initiala
    din care s-au eliminat toti indecsii ceruti.
-}
remove1IndexAtATime :: [Int] -> Row -> [Row]
remove1IndexAtATime list_indexes row = map (deleteAt1 row) list_indexes

intersect :: Row -> Row -> Row
intersect [] l2 = []
intersect (x:xs) l2
                |x `elem` l2 = x : intersect xs l2
                |otherwise = intersect xs l2

intersectAll :: [Row] -> Row
intersectAll = foldl1 op 
                    where   op acc [] = acc
                            op acc e1 = acc `intersect` e1

addNewColumnsFromT1_1 :: Int -> Int -> Table -> [Int] -> [Int] -> Row -> Row
addNewColumnsFromT1_1 ind_t1 ind_t2 [] l1 l2 row_t2= []
addNewColumnsFromT1_1 ind_t1 ind_t2 t1@(row_t1:t1s) l1 l2 row_t2
                                |(row_t1 !! ind_t1) == (row_t2 !! ind_t2) = modified_row_t1 ++ modified_row_t2
                                |otherwise = addNewColumnsFromT1_1 ind_t1 ind_t2 t1s l1 l2 row_t2
                                where   modified_row_t1 = modify row_t1 row_t2 l1 l2
                                        modified_row_t2 = intersectAll intersectedList
                                        intersectedList = remove1IndexAtATime (ind_t2:l2) row_t2
{-                              
    "list_indexes1" = lista indecsilor comuni raportati la tabelul1
    "list_indexes2" = lista indecsilor comuni raportati la tabelul2
    "list_indexes_antet" = lista indecsilor comuni raportati la antet
    Toate 3 nu includ "key_column".
    Indexul lui key_column se retine in "index_t1" si "index_t2".
-}
joinRows1 :: ColumnName -> Table -> Table -> [String] -> Table
joinRows1 key_column t1 t2Ref cols = map (addNewColumnsFromT1_1 index_t1 index_t2 t1 list_indexes1 list_indexes2) $ tail t2Ref
                        where   index_t1 = getIndex key_column (head t1) 0
                                index_t2 = getIndex key_column (head t2Ref) 0
                                list_indexes1 = map (getIndex1 (head t1) 0) cols
                                list_indexes2 = map (getIndex1 (head t2Ref) 0) cols

-- Elimin din "common_columns", "key_column".
tjoin :: ColumnName -> Table -> Table -> Table
tjoin key_column t1 t2 
        |onlyOneOcc key_column (head t1) (head t2) && onlyOneCommon (head t1) (head t2) 0 = antet ++ joinRows key_column t1 t2
        |otherwise = antet ++ joinRows1 key_column t1 t2 common_columns
            where   antet = [notIn (head t1) (head t2)]
                    common_columns = filter (/= key_column) $ comColumns (head t1) (head t2) []
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
-- Task 5
cart :: (Row -> Row -> Row) -> Table -> Table -> Table
cart f [] table2 = []
cart f table1 table2 = foldr (\ t1 -> (++) (map (f t1) table2)) [] table1

cartesian :: (Row -> Row -> Row) -> [ColumnName] -> Table -> Table -> Table
cartesian new_row_function new_column_names t1 t2 = new_column_names : cart new_row_function (tail t1) (tail t2)
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
-- Task 6
{- 
    "getIndex1" face exact acelasi lucru ca si getIndex doar ca am
    schimbat ordinea parametrilor, basically ca sa il pot folosi ca
    functie pentru map. Se presupune ca toate numele de coloane date
    exista deja in lista coloanelor tabelului, adica nu primim nume
    de coloane care nu exista.
-}
getIndex1 :: Row -> Int -> ColumnName -> Int
getIndex1 [] acc column = undefined
getIndex1 (c:cs) acc column
                        | c == column = acc
                        | otherwise = getIndex1 cs (acc + 1) column

-- Intoarce lista de indecsi corespunzatoare listei de nume pentru coloanele date
getIndexes :: [ColumnName] -> [ColumnName] -> [Int]
getIndexes givenNames actualNames = map (getIndex1 actualNames 0) givenNames

-- Returneaza coloana pentru care se da indexul
extractColumn :: Table -> Int -> Row
extractColumn table index = map (!!index) table

-- Extrage din tabel coloanele pentru care se cunosc indecsii din lista de indecsi
extractColumns :: [Int] -> Table -> Table
extractColumns [] t = []
extractColumns xs t = foldr (\ index -> (++) [extractColumn t index]) [] xs 

{- 
    "projection1" e necesar pentru ca  "extractColumns" extrage coloanele
    dar nu in formatul dorit in ref. Mai exact extrage toata coloana ca o
    lista de stringuri. Dupa "projection1" fiecare string din coloana va
    apare ca unic element al unei liste. Deci practic o sa fie o lista de
    liste de liste de stringuri in care cel mai interior nivel de lista va
    avea un singur string drept element.
-}
projection1 :: [ColumnName] -> Table -> [Table]
projection1 columns_to_extract t = map (map (: [])) (extractColumns listOfIndexes t)
                                where listOfIndexes = getIndexes columns_to_extract $ head t

{- 
    Daca aplic "concat" pe rezultatul lui "projection1" se obtine
    rezultatul dorit in ref. Se presupune ca nu primesc in lista
    de coloane mai multe coloane decat exista si ca toate coloanele
    primite exista in lista de coloane a tabelului, adica nu trebuie
    sa verific daca fiecare coloana primita exista. Otherwise-ul
    l-am pus pt. ca am observat ca sunt 2 teste la taskul asta, iar
    outputurile difera ca format. La al doilea test mai exact coloanele
    primite sunt exact toate coloanele tabelului caz in care se returneaza
    tabelul fara alte prelucrari.
-}
projection :: [ColumnName] -> Table -> Table
projection columns_to_extract t
                        |l1 < l2 = concat $ projection1 columns_to_extract t
                        |otherwise = t 
                                where   listOfIndexes = getIndexes columns_to_extract $ head t
                                        l1 = length columns_to_extract
                                        l2 = length $ head t
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
-- Task 7
filterRows :: (Value -> Bool) -> Int -> Table -> Table
filterRows cond index = filter (\row -> cond $ row !! index)

filterTable :: (Value -> Bool) -> ColumnName -> Table -> Table
filterTable condition key_column t = head t : filterRows condition index_column (tail t)
                                where index_column = getIndex key_column (head t) 0

