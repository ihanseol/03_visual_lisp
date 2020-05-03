(defun c:000()
 (setvar   "cmdecho" 0)
 (if (= (tblsearch "layer" "COL") nil) 
     ;layer 1 이 있는지 없는지 확인 없으면 만든다.
     (command "layer" "m" "COL" "c" "2" "" "lt" "continuous" "" ""))
     ;            레이어이름    레이어색상      레이어선종류

 (if (= (tblsearch "layer" "CEN") nil) 
     (command "layer" "m" "CEN" "c" "1" "" "lt" "CENTER" "" ""))

 (if (= (tblsearch "layer" "DIM") nil) 
     (command "layer" "m" "DIM" "c" "7" "" "lt" "continuous" "" ""))

 (if (= (tblsearch "layer" "LAN") nil) 
     (command "layer" "m" "LAN" "c" "1" "" "lt" "continuous" "" ""))

 (if (= (tblsearch "layer" "SYM") nil) 
     (command "layer" "m" "SYM" "c" "7" "" "lt" "continuous" "" ""))

 (if (= (tblsearch "layer" "HAT") nil) 
     (command "layer" "m" "HAT" "c" "5" "" "lt" "continuous" "" ""))

 (if (= (tblsearch "layer" "HAT1") nil) 
     (command "layer" "m" "HAT1" "c" "8" "" "lt" "continuous" "" ""))

 (if (= (tblsearch "layer" "TEX") nil) 
     (command "layer" "m" "TEX" "c" "3" "" "lt" "continuous" "" ""))

 (if (= (tblsearch "layer" "WID") nil) 
     (command "layer" "m" "WID" "c" "4" "" "lt" "continuous" "" ""))

 (if (= (tblsearch "layer" "COR") nil) 
     (command "layer" "m" "COR" "c" "4" "" "lt" "continuous" "" ""))

 (if (= (tblsearch "layer" "FORM") nil) 
     (command "layer" "m" "FORM" "c" "7" "" "lt" "continuous" "" ""))

 (if (= (tblsearch "layer" "FUR") nil) 
     (command "layer" "m" "FUR" "c" "4" "" "lt" "continuous" "" ""))

 (if (= (tblsearch "layer" "FIN") nil) 
     (command "layer" "m" "FIN" "c" "5" "" "lt" "continuous" "" ""))

 (if (= (tblsearch "layer" "HID") nil) 
     (command "layer" "m" "HID" "c" "7" "" "lt" "HIDDEN" "" ""))


(princ)
)


(defun c:0()
  (ssget)
  (command"LAYER" "S" "0" "")
  ;현재 레이어를 0 레이어로 바꾼다.
  (command "chprop" "p" "" "la" "0" "")
)
(defun c:1()
  (ssget)
  (command"LAYER" "S" "COL" "")
  (command "chprop" "p" "" "la" "COL" "")
)
(defun c:2()
  (ssget)
  (command"LAYER" "S" "COR" "")
  (command "chprop" "p" "" "la" "COR" "")
)
(defun c:3()
  (ssget)
  (command"LAYER" "S" "DIM" "")
  (command "chprop" "p" "" "la" "DIM" "")
)
(defun c:4()
  (ssget)
  (command"LAYER" "S" "FUR" "")
  (command "chprop" "p" "" "la" "FUR" "")
)
(defun c:5()
  (ssget)
  (command"LAYER" "S" "LAN" "")
  (command "chprop" "p" "" "la" "LAN" "")
)
(defun c:6()
  (ssget)
  (command"LAYER" "S" "SYM" "")
  (command "chprop" "p" "" "la" "SYM" "")
)
(defun c:7()
  (ssget)
  (command"LAYER" "S" "HAT" "")
  (command "chprop" "p" "" "la" ""HAT"")
)
(defun c:8()
  (ssget)
  (command"LAYER" "S" "TEX" "")
  (command "chprop" "p" "" "la" "TEX" "")
)
(defun c:9()
  (ssget)
  (command"LAYER" "S" "WID" "")
  (command "chprop" "p" "" "la" "WID" "")
)
(princ)