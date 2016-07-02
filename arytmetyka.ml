(* Dla (x, y) jeśli x<=y to możliwymi rozwiązaniami jest zbior [x, y] *)
(* Jeśli x>y to możliwymi rozwiązaniami jest suma zbiorów (-infinity, y] i [x, infinity)*)
(* Mówimy wtedy, że rozwiązaniem jest zbiór rozłączny *)
type wartosc = float * float

(* Funkcja łacząca zbiory rozłączne, jeśli się pokrywają *)
let polacz_zbiory (x, y) = if(x<=y) then (neg_infinity, infinity) else (x, y)

let wartosc_dokladnosc x p = if (x>0.) then (x -. x*.(p/.100.), (x +. x*.(p/.100.))) else (x +. x*.(p/.100.), (x -. x*.(p/.100.)))
let wartosc_od_do x y = (x, y)
let wartosc_dokladna x = (x, x)

let in_wartosc (x, y) w =
  let in_wartosc_rozlaczne =
    if(w<=y || w>=x) then true else false in
  let in_wartosc_nie_rozlaczne =
    if(w>=x && w<=y) then true else false in
  if(x<=y) then in_wartosc_nie_rozlaczne else in_wartosc_rozlaczne


let min_wartosc (x, y) =
  if(x>y) then neg_infinity else x

let max_wartosc (x, y) =
  if(x>y) then infinity else y

let sr_wartosc w = (max_wartosc w+.min_wartosc w)/.2.

(* Funkcje pomocnicze zwracające odpowiednio mniejszą, oraz większą z podanych wartości *)
let min a b = if (a < b || (compare b nan = 0) ) then a else b
let max a b = if (a > b || (compare b nan = 0) ) then a else b

(* Przy wykonywaniu każdego z działań w pierwszej kolejności sprawdzane jest *)
(* czy, któryś z argumentów nie jest nieokreślony czyli równy (nan, nan)     *)

let plus (x1, y1) (x2, y2) =
  let plus_dwa_rozlaczne = (neg_infinity, infinity) in
  let plus_jeden_rozlaczny =
    polacz_zbiory (x1 +. x2, y1 +. y2) in
  let plus_bez_rozlacznych = (x1 +. x2, y1 +. y2) in
  if ((compare x1 nan = 0) || (compare x2 nan = 0)) then (nan, nan) else
  if(x1<=y1) then
    if(x2<=y2) then plus_bez_rozlacznych else plus_jeden_rozlaczny
  else
    if(x2<=y2) then plus_jeden_rozlaczny else plus_dwa_rozlaczne

let minus (x1, y1) (x2, y2) =
  let minus_dwa_rozlaczne = (neg_infinity, infinity) in
  let minus_jeden_rozlaczny =
    polacz_zbiory (x1 -. y2, y1 -. x2) in
  let minus_bez_rozlacznych = (x1 -. y2, y1 -. x2) in
  if ((compare x1 nan = 0) || (compare x2 nan = 0)) then (nan, nan) else
  if(x1<=y1) then
    if(x2<=y2) then minus_bez_rozlacznych else minus_jeden_rozlaczny
  else
    if(x2<=y2) then minus_jeden_rozlaczny else minus_dwa_rozlaczne

let razy (x1, y1) (x2, y2) =
  let razy_bez_rozlacznych = (min (min (x1*.x2) (x1*.y2)) (min (y1*.x2) (y1*.y2)), max (max (x1*.x2) (x1*.y2)) (max (y1*.x2) (y1*.y2))) in
  let razy_jedne_rozlaczne (x1, y1) (x2, y2) =
    if (x2>0. && y2>0.) then
      if (x1<0.) then
        polacz_zbiory ((x1*.y2), (y1*.x2)) else
	if (y1<0.) then polacz_zbiory ((x1*.x2), (y1*.x2)) else polacz_zbiory ((x1*.x2), (y1*.y2))
    else if (x2<0. && y2<0.) then
      if (x1<0.) then
	polacz_zbiory ((y1*.x2), (x1*.y2)) else
	if (y1<0.) then polacz_zbiory ((y1*.y2), (x1*.y2)) else polacz_zbiory ((y1*.x2), (x1*.y2))
    else if (x2=0.) then
      if (y2=0.) then (0., 0.) else (neg_infinity, infinity)
    else (neg_infinity, infinity) in
  let razy_dwa_rozlaczne =
    if (x1<=0. || x2<=0. || y1>=0. || y2>=0.) then (neg_infinity, infinity)
    else (min (y1*.y2) (x1*.x2), max (y1*.x1) (x2*.y2)) in
  if ((compare x1 nan = 0) || (compare x2 nan = 0)) then (nan, nan) else
  if ((x1=0. && y1=0.) || (x2=0. && y2=0.)) then (0., 0.) else
  if (x1<=y1) then 
    if (x2<=y2) then razy_bez_rozlacznych else razy_jedne_rozlaczne (x2, y2) (x1, y1)
  else if (x2<=y2) then razy_jedne_rozlaczne (x1, y1) (x2, y2) else razy_dwa_rozlaczne

(* Korzystamy z tego, że (a/b)=(a*(1/b)) *)
let podzielic (x1, y1) (x2, y2) =
  let odwrotnosc =
      if (x2=0.) then ((1./.y2), (infinity)) else
	if (y2=0.) then ((neg_infinity), (1./.x2)) else
	  if (x2=neg_infinity && y2=infinity) then (neg_infinity, infinity) else
          ((1./.y2), (1./.x2)) in
  if ((compare x1 nan = 0) || (compare x2 nan = 0)) then (nan, nan) else
  if (x2=0. && y2=0.) then (nan, nan) else razy (x1, y1) odwrotnosc
