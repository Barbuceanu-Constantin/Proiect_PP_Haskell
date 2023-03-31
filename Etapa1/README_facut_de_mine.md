Etapa1 PP Proiect (Barbuceanu Constantin   322CB)
=============================

## 1. **Task1**
    Primeste un tabel "eight_hours" si intoarce un tabel care are coloanele "Name" si "Average Number Of Steps".

    Am folosit foldl ca sa parcurg tabelul primit ca parametru de la stanga la dreapta si pt. fiecare linie
    am facut media celor 8 valori (numere de pasi).

    A trebuit sa fac functii auxiliare precum (read1 :: String -> Float) sau altele in incercarea de a obtine formatul dorit pt. numere si de a face codul mai lizibil.

## 2. **Task2**
    Are 3 subtaskuri fiecare cu functia lui principala.

    Am incercat sa dau nume sugestive functiilor auxiliare si parametrilor pe care le primesc acestea, chiar daca as fi putut sa le denumesc pe unele mult mai scurt creand impresia unui cod mai scurt.

## 3. **Task3**
    Provocarea aici a constat in a reusi sa fac media tuturor valorilor de pe aceeasi coloana (practic in a extrage numerele de pe acceasi coloana).

    Am creat functia "foldl_With_2_Accumulators" care simuleaza oarecum comportamentul lui foldl. Practic functia asta creeaza intr-un acumulator o lista in care fiecare element este o coloana din cele 8 pe care se vrea sa se faca media.

    Media, suma si conversia la Float se fac in "get_Sums_Per_Days" si functia principala.

## 4. **Task4**
    Am creat 3 functii "VeryActive", "FairlyActive", "LightlyActive". Fiecare creeaza linia corespunzatoare din tabelul de output, toate 3 fiind concatenate in functia principala.

    Am mai creat 3 predicate "range1", "range2", "range3" care verifica in ce interval se afla numarul total de pasi al unei persoane (0-50), (50-100), (100-500).

    Mai e si functia "enhanced_length" care pp-zis. numara cate persoane se afla in cele 3 intervale pe baza parametrilor primiti.

## 5. **Task5**
    Taskul a fost usor odata ce am aflat ca pot folosi sortBy. Practic de aici broadly speaking a trebuit sa mai implementez doar 2 functii "compare" fiecare dupa cate un criteriu de comparare din enunt.

## 6. **Task6**
    Din nou cu sortBy si doua functii de comparare plus inca niste functii cum ar fi "averageFirst4", "averageLast4" sau "convertEntry" care pp-zis transforma o linie din tabelul initial intr-o linie din tabelul final.

## 7. **Task7**
    Usor de scris. Doar 2 mapuri.

## 8. **Task8**
    Destul de usor de implementat get_sleep_total. Am facut si functia rmap, dar am impresia ca nu este testata de checker. Prin urmare am testat-o eu local manual scriind functia f2 pe care o dau lui rmap drept parametru.