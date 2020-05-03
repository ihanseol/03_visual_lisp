; 아키모아 운영진 "행복한하루"
; http://cafe.daum.net/archimore
; 내용이 없는 text -> 유형객체 삭제하기.
; 2007.08.13  

(defun c:ghost(/ k j a ss ss1 ss2 en ed etn x10 x11 lis n dissum dis)
  (setvar "cmdecho" 0)
  (prompt "\n>>도면 정리 하기..")
  (prompt "\n>>내용이 없는 유형객체 삭제하기<text/mtext/line/pline/node>..")
  (setq k 0 j 0 a nil)
  (setq ss (ssget "x" (list (cons 0 "text,mtext") (cons 1 ""))))
  (setq ss1 (ssget "x" (list (cons 0 "line,lwpolyline"))))
  (setq ss2 (ssget "x" (list (cons 0 "point"))))
  (command "undo" "be")
  (if ss1 (progn 
   (repeat (sslength ss1)
    (setq en (ssname ss1 k)
          ed (entget en)
          etn (cdr (assoc 0 ed)))
    (cond ((= etn "LINE")
           (setq x10 (cdr (assoc 10 ed))
                 x11 (cdr (assoc 11 ed)))
           (if (= (distance x10 x11) 0) (progn (command "erase" en "") (setq j (1+ j)))))
          ((= etn "LWPOLYLINE") 
           (setq lis (GetPolyVtx ed))
           (setq n 0 dissum 0)
           (repeat (1- (length lis))
            (setq dis (distance (nth n lis) (nth (1+ n) lis))) 
            (setq dissum (+ dissum dis))
            (setq n (1+ n))
           );repeat
           (if (= dis 0) (progn (command "erase" en "") (setq j (1+ j)))))
     );cond     
    (setq k (1+ k))      
   );repeat
  ));if progn
  (if ss2 
   (progn 
    (while (= a nil) 
     (setq a (strcase (getstring "\nPoint<node> 을 삭제하시겠습니까? Y & N :")))
     (cond ((= a "Y") (progn (command "erase" ss2 "") (setq a 1)))
           ((= a "N") (setq a 2))
           (t (setq a nil))
     );cond
    );while
   );progn
  );if 
  (if ss (progn (command "erase" ss "") (princ "\n->내용이 없는 text ") 
         (princ (sslength ss)) (princ "개") (princ " 삭제하였습니다.")))
  (if (> j 0) (progn (princ "\n->선길이 0 인 객체 ") (princ j) (princ "개를") (princ " 삭제하였습니다.")))
  (if (= a 1) (princ "\n->Point<node> 을 삭제하였습니다"))
  (if (and (= ss nil) (= j 0)) (princ "\n>>도면관리를 잘하시는군여!! ^^"))
  (command "undo" "e")
  (princ)
);defun


(defun GetPolyVtx(EntList) 
  (setq VtxList '()) 
  (foreach x EntList 
   (if (= (car x) 10) 
    (setq VtxList (append VtxList (list (cdr x)))) 
   ) 
  ) 
VtxList 
)