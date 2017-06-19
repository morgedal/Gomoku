# Gomoku
Gra w kółko i krzyżyk do 5 na planszy 19*19.

Są trzy wersje funkcji oceniającej do wyboru, wystarczy zakomentować pozostałe i wybrać jedną. Wszystkie są do bani, bo albo jest to losowość albo coś co gra sensowniej, ale za to po kilku ruchach zaczyna tak długo obliczać następny, że szkoda gadać.

Żeby rozpocząć rozgrywkę AI vs AI należy uruchomić funkcję main (bez żadnego parametru), lub funkcję gameLoop ( ażeby rozpocząć grę od wybranego pola : gameLoop (insertCircle x_pos y_pos createBoard) Cross , z ewentualną zamianą Circle i Cross  )
 
Żeby zagrać przeciwnik vs AI ( co właściwie jest bez sensu przy takim AI ) należy wykonywać kolejno w ghci komendy:

let board = insertCircle x y previousBoard  //za pierwszym razem zamiast previousBoard dajemy createBoard

let previousBoard = makeMove Cross board

checkIfGameOver previousBoard

previousBoard  // w celu ewentualnego wyświetlenia


I tak w kółko, aż wygramy, albo nam się znudzi czekanie na ruchy przeciwnika.
