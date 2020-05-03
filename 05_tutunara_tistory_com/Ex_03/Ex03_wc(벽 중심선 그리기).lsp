;;====================================================================
;  벽체 중심선 그리기 (2007.6.주말농부)
;  ->두선의 중간에 중심선을 그리는 명령어(평행유무 관계없음)
;    라인일때만 인식됨 / 연속선택 기능
;  ->중심선 레이어 선택기능 추가(2007.7.1)
;;------ wall center draw --------------------------------------------
(defun c:wc(/ os ln en1 en2 en3 ed1 ed2 p1s p1e p2s p2e d1 d2 cps cpe)
;->*error* start
 (defun *error* (msg)(princ "error: ")(princ msg)
 (setvar "osmode" os)(setvar "clayer" cl)
 (princ))
;-<*error* end
   (setq os (getvar "osmode") cl (getvar "clayer"))
   (setq en1 nil en2 nil)
   (setvar "osmode" 0)
   (prompt " 벽체 중심선 그리기...")
   (setq en3 (car (entsel "\n중심선레이어 선택<현재레이어>: ")))
   (if (= en3 nil) (setq ln cl)
       (setq ln (cdr (assoc 8 (entget en3)))) )
   (setvar "clayer" ln)
   (setq en1 (car (entsel "\n첫번째선->")))
   (if en1 (setq en2 (car (entsel " 두번째선->"))))
(while en2
   (setq ed1 (entget en1))
   (setq ed2 (entget en2))
   (if (and (= (cdr (assoc 0 ed1)) "LINE")
            (= (cdr (assoc 0 ed2)) "LINE"))
      (progn
         (setq p1s (cdr (assoc 10 ed1)) p1e (cdr (assoc 11 ed1)))
         (setq p2s (cdr (assoc 10 ed2)) p2e (cdr (assoc 11 ed2)))
         (if (< (distance p1s p2e) (distance p1s p2s))
             (setq tem p2s p2s p2e p2e tem) )
         (setq d1 (/ (distance p1s p2s) 2.0))
         (setq d2 (/ (distance p1e p2e) 2.0))
         (setq cps (polar p1s (angle p1s p2s) d1))
         (setq cpe (polar p1e (angle p1e p2e) d2))
         (command "line" cps cpe "") )
      (prompt "..라인을 선택할것")
   );if end
   (setq en1 nil en2 nil)
   (setq en1 (car (entsel "\n첫번째선->")))
   (if en1 (setq en2 (car (entsel " 두번째선->"))))
);while end
   (setvar "osmode" os) (setvar "clayer" cl)
(prin1))