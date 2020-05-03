; 아키모아 운영진 "행복한하루"
; http://cafe.daum.net/archimore
; 구조 창문 개구부 표시
; 2007.03.09 1차수정 2007.07.25 

(defun c:sb(/ P1 P2 P3 P4 )
;->*error* start
 (defun *error* (msg)(princ "error: ")(princ msg)
 (setvar "osmode" os) (setvar "orthomode" orth)
 (princ))
;-<*error* end

  (setq os (getvar "osmode")
        orth (getvar "orthomode"))   
  (setq p1 (getpoint "\nFirst point:"))
  (setvar "orthomode" 0)
  (while p1
    (setq p2 (getpoint p1 "\nSecond point:")) 
    (setq p3 (list (car p1) (cadr p2))) 
    (setq p4 (list (car p2) (cadr p1)))
    (setvar "osmode" 0) 
    (if (> (distance p1 p3) (distance p1 p4)) 
        (command "pline" p1 p3 p4 p2 "c")
        (command "pline" p1 p4 p3 p2 "c")
    );if
    (setvar "osmode" os)
    (setq p1 (getpoint "\nFirst point:")) 
  );while
(setvar "orthomode" orth)
(princ)
);defun 

