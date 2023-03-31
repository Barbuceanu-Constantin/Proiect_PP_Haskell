Etapa2 PP Proiect (Barbuceanu Constantin   322CB)
=============================

## 1. **Task1**
Se cere sortarea tabelului primit ca parametru dupa o coloana lexicografic pentru stringuri si crescator pentru numere. Daca valoarea lipseste se considera prima.

Taskul a fost oarecum greu. A trebuit sa folosesc 13 functii auxiliare in incercarea de a acoperi toate cazurile si de a scrie cod compact. Cateva lucruri notabile:

* Am refolosit functiile "read1" si "read2" de la etapa 1.
* Am creat o functie "getIndex" care intoarce indexul unei coloane data dupa nume. Formatul acestei functii il voi folosi si la un task ulterior dar cu o ordine diferita a parametrilor astfel incat sa o pot pasa ca functie unui map.
* Am facut diverse functii de comparare care se apeleaza incremental pana la cazul de baza, respectiv daca este string sau numar. "compOnColumn" apeleaza: "comp1", "comp2", "compLexic". Fiecare din astea 3 apeleaza "compOnFstCol" care apeleaza "compInt", "compFloat", "compString".
* Am mai facut "checkBothInt", "checkBothFloat" care verifica tipul datelor cu ajutorul functiei "readMaybe". 

## 2. **Task2**
Taskul a fost usor si s-a rezolvat repede. Am facut o functie "sameColumns" care intoarce un Bool si verifica daca 2 tabele au aceeasi grila de coloane si in functie de rezultatul ei am adaugat liniile din t2 la finalul lui t1 sau am lasat t1 intact.

## 3. **Task3**
Din nou un task usor. Functiile ajutatoare principale sunt "appendRows" si "padd". Paddingul se produce pe tabelele de dimensiune mai mica ca apoi sa poata fi date ca parametru lui "appendRows" care are la baza un zipWith.

## 4. **Task4**
Acesta a fost cel mai greu task din punctul meu de vedere. Dificultatea a venit din mai multe surse:

* Faptul ca enuntul trebuie digerat un oarecare timp pana sa intelegi cam ce urmareste, cum ar arata inputurile si outputul.
* Faptul ca enuntul introduce cazul suplimentar cand pe langa "key_column" mai exista si alte coloane cu acelasi nume in cele doua tabele si din ce am observat acest caz nu se testeaza explicit. Am rezolvat si acest caz, dar testand eu de mana, ceea ce probabil ca a avut beneficiile sale.

Rezolvarea taskului 4 se imparte in 2 ramuri in codul meu:
1) cazul cand "key_column" este singura coloana comuna intre cele doua tabele
2) cazul cand exista si alte coloane comune pe langa "key_column" intre cele 2 tabele.

Cateva lucruri esentiale pe care le-am facut ca sa rezolv taskul:
* Extrag numele coloanelor comune din cele 2 tabele intr-o lista si elimin dupa caz "key_column" din aceasta lista.
* Pe baza acestei liste obtin "list_indexes1" si "list_indexes2" care sunt doua liste de Int care contin indecsii corespunzatori coloanelor comune raportat la tabelul1 sau tabelul2

ATENTIE!! : Din ce am observat ordinea liniilor se bazeaza pe t2, iar ordinea coloanelor pe t1 (adica incepe cu alea din t1).

* Daca doar "key_column" este coloana comuna, operatia de baza de "merge-uire" pe linii pentru liniile cu aceeasi valoare pe coloana este : "row_t1 ++ (deleteAt ind_t2 row_t2)". "deleteAt" sterge valoarea corespunzatoare coloanei "key_column" din linia tabelului 2 pentru ca ea se regaseste deja la inceputul lui "row_t1".
* Daca suntem pe cazul cand exista si alte coloane comune am folosit functiile "modify", "intersectAll" si "remove1IndexAtATime". "modify" modifica valorile din "row_t1" pe baza faptului ca cele din tabelul 2 au prioritate si drept de suprascriere, iar "intersectAll" si "remove1IndexAtATime" se ocupa de eliminarea coloanelor comune din "row_t2", stiind ca ele exista deja in "row_t1". Principiul de eliminare este ca daca elimini pe rand cate o coloana si intersectezi toate listele rezultate in urma eliminarii unui singur element din initiala, obtii lista finala.

## 5. **Task5**
Taskul a fost simplu. Se cerea un produs cartezian. S-a rezolvat in mai putin de 10 linii.

## 6. **Task6**
Aici am refolosit functia "getIndex", drept "getIndex1" cu o alta ordine a parametrilor. Mai mult, am generalizat, facand functia "getIndexes" care intoarce o lista de indecsi daca este vorba de mai multe coloane.

Taskul cere sa extragi coloanele date ca nume intr-o lista de String-uri.

## 7. **Task7**
Task usor. Am folosit din nou "getIndex". Se cerea sa se filtreze liniile bazat pe o conditie data aplicata unei coloane date.

#Observatii :
 * As fi putut probabil sa fac pe alocuri codul si mai compact, intrucat in cateva locuri folosesc functii pe care deja le-am facut dar usor modificate cum ar fi la ordinea parametrilor de exemplu. Am facut asta ca sa pot folosi functia intr-un map. Ideea este ca atunci cand am scris prima oara nu aveam idee cum voi refolosi mai tarziu ce scriam. Prin urmare cand mi-am dat seama ca pot refolosi, ar fi trebuit sa modific si codul deja existent ca sa schimb functia initiala. Asa ca am ales varianta mai usoara pe moment de a rescrie functia cu un alt nume si o alta ordine a parametrilor.
 * La taskul 4, am rezolvat si cazul in care exista mai multe coloane cu acelasi nume pe langa "key_column", in cele 2 tabele. Se poate verifica manual.
 * La etapa asta am observat pentru prima oara utilitatea sugestiilor de scriere mai eficienta si compacta a codului lansate de extensia de haskell in vscode. Chiar am invatat si mi-am clarificat multe lucruri uitandu-ma pe sugestii.
 * Exista posibilitatea ca in unele locuri numele functiilor sau variabilelor, din incercarea mea de a fi foarte explicite si de a ma ajuta sa nu pierd ideea, sa fie usor lungi.