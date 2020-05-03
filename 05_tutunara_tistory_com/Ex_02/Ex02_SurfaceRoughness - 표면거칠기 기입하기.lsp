;; 표면 거칠기 (삼각형) 입력 lisp (Surface roughness)
;; revision by nadau (2009.05.12)
;; 스냅자석 오류 개선 by nadau (2010-0419)

(defun dtr (a)
    (* pi (/ a 180.0)))

(defun rtd (a)
    (/ (* a 180.0) pi))

(defun c:sr (/ skk skk1 num bas p1 p2 p3 p4 p5 p6 p7 e1 e2 ocmde ooth osm cla col)

;아래 두줄은 명령 실행중 취소를 하거나 에러 발생시 에러 메세지를 띄우고 osmode, clayer, orthomode 를 초기값으로 되돌림 
(defun *error* (msg)(princ "error: ")(princ msg) 
(setvar "osmode" osm) (setvar "clayer" cla)(setvar "orthomode" ooth) (princ) ) 
;;에러 핸들러 끝 

   (princ "\n ** 다듬질 기호 그리기 **")

   (setq ocmde (getvar "cmdecho"))
   (setq ooth (getvar "orthomode"))
   (setq osm (getvar "osmode"))
   (setq cla (getvar "clayer")) 
   (setq col (getvar "cecolor")) 

   (princ "\n 크기를 입력하시오 [") (princ (getvar "dimscale")) (princ "] : ")
   (setq skk1 (getreal) )
   ( if (= skk1 nil) (setq skk1 (getvar "dimscale")) )
   (setq skk (* 3.0 skk1))

   (setq ki  ( * 1 skk1))

   (setvar "cmdecho" 0)
   (command "osnap" "nea")
   (command "ortho" "on")

   (setq num (getint   "\n 거칠기 종류를 선택 하시오.( 1[▽], 2[▽▽], 3[▽▽▽], 4[G], 5[FR], 6[FB] ) : [2] ")) 

   (setq bas (getpoint "\n 기준점을 지정 하시오.: "))

; 3번 레이어 없으면 생성
 (if (= (tblsearch "layer" "3") nil) 
     (command "layer" "m" "3" "c" "3" "" "lt" "continuous" "" ""))
; 3번 레이어에 도시하기 위해서 임시로 3번 레이어로 변경
 (command "layer" "s" "3" "")            ;; Layer Setting  ( Layer :: [3] )

   (command "osnap" "off")
   
   (if (= num nil) (setq num 2)) 
   (cond
   ((= num 1)
      (setq p1 (polar bas (dtr  60) skk)) 
      (setq p2 (polar bas (dtr 120) skk))
      (command "pline" bas p1 p2 "c")     (setq e1 (entlast))
      (command "rotate" e1 "" bas pause))
   ((= num 2)
       (setq p1 (polar bas (dtr    0) (/ skk 2)))
       (setq p2 (polar p1  (dtr   60) skk))
       (setq p3 (polar p1  (dtr  120) skk))
       (setq p4 (polar p3  (dtr -120) skk))
       (setq p5 (polar p4  (dtr  120) skk))
       (command "pline" p1 p2 p5 p4 p3 "c") (setq e1 (entlast))
       (command "rotate" e1 "" bas pause))
    ((= num 3)
       (setq p1 (polar bas (dtr   60) skk))
       (setq p2 (polar p1  (dtr  -60) skk))
       (setq p3 (polar p2  (dtr   60) skk))
       (setq p4 (polar bas (dtr  120) skk))
       (setq p5 (polar p4  (dtr -120) skk))
       (setq p6 (polar p5  (dtr  120) skk))
       (command "pline" bas p1 p2 p3 p6 p5 p4 "c") (setq e1 (entlast))
       (command "rotate" e1 "" bas pause))
    ((= num 4)
       (setq p1 (polar bas (dtr   60) skk))
       (setq p2 (polar p1  (dtr  -60) skk))
       (setq p3 (polar p2  (dtr   60) skk))
       (setq p4 (polar bas (dtr  120) skk))
       (setq p5 (polar p4  (dtr -120) skk))
       (setq p6 (polar p5  (dtr  120) skk))
       (setq p7 (polar bas (dtr   90) skk))
       (command "text" "j" "c" p7 ki "" "G") (setq e2 (entlast))
       (command "pline" bas p1 p2 p3 p6 p5 p4 "c") (setq e1 (entlast))
       (command "rotate" e1 e2 "" bas pause))
    ((= num 5)
       (setq p1 (polar bas (dtr   60) skk))
       (setq p2 (polar p1  (dtr  -60) skk))
       (setq p3 (polar p2  (dtr   60) skk))
       (setq p4 (polar bas (dtr  120) skk))
       (setq p5 (polar p4  (dtr -120) skk))
       (setq p6 (polar p5  (dtr  120) skk))
       (setq p7 (polar bas (dtr   90) skk))
       (command "text" "j" "c" p7 ki "" "FR") (setq e2 (entlast))
       (command "pline" bas p1 p2 p3 p6 p5 p4 "c") (setq e1 (entlast))
       (command "rotate" e1 e2 "" bas pause))
    ((= num 6)
       (setq p1 (polar bas (dtr   60) skk))
       (setq p2 (polar p1  (dtr  -60) skk))
       (setq p3 (polar p2  (dtr   60) skk))
       (setq p4 (polar bas (dtr  120) skk))
       (setq p5 (polar p4  (dtr -120) skk))
       (setq p6 (polar p5  (dtr  120) skk))
       (setq p7 (polar bas (dtr   90) skk))
       (command "text" "j" "c" p7 ki "" "FB") (setq e2 (entlast))
       (command "pline" bas p1 p2 p3 p6 p5 p4 "c") (setq e1 (entlast))
       (command "rotate" e1 e2 "" bas pause))) 


 (setvar "osmode" osm)
 (setvar "orthomode" ooth)
 (setvar "cmdecho" ocmde)
 (setvar "cecolor" col) 
 (setvar "clayer" cla)

(princ))

