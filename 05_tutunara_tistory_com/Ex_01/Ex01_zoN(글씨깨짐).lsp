;;===============================================================
;  z좌표 없애기(1996.9 주말농부)
;  ->z값을 0으로 바꾸는 명령어
;  ->진행과정 시각화(2006.11)
;  ->polyline 적용(2007.8.8)
;  ->dimension 적용가능(2008.5.30)
; PolyLine모듈수정(2009.11.10 하얀제비)
;;------ z-axis 0 -----------------------------------------------
(defun c:zoN(/ ss ssn k ptz en en1 ed ed1 spt ept ptz sptx spty
              eptx epty sp13 sp14 FigEntVla CoordL CoordLT i )
  (vl-load-com)
  ;;;Undo 초기화시작
  (vla-StartUndoMark (vla-get-ActiveDocument (vlax-get-ACAD-Object)))
  ;;;Undo 초기화시작
   (prompt "\nCommand: 2D Entity...")
   (prompt "\nZ값을 0으로 하고자하는 대상 선택-> ")
   (setq ss (ssget))
   (setq ssn (sslength ss))
   (setq k 0 ptz 0)(terpri)
   (repeat ssn
      (setq en (ssname ss k))
      (setq ed (entget en))
      (if (= (cdr (assoc 0 ed)) "POLYLINE")
         (progn ;->polyline yes
	   (setq FigEntVla (vlax-ename->vla-object en))
	   (setq CoordL (vlax-safearray->list  (variant-value (vlax-get-property FigEntVla 'coordinates))))
	   (setq i 0 CoordLT '())
	   (while (> (length CoordL) i)
	     (cond
	       ((= (rem (+ i 1) 3) 0)
		(setq CoordLT (append CoordLT (list 0.0)))
	       )
	       (T
		(setq CoordLT (append CoordLT (list (nth i CoordL))))
	       )
	     )
	     (setq i (1+ i))
	   );;;while
	   (vla-put-coordinates FigEntVla (vlax-safearray-fill (vlax-make-safearray vlax-vbDouble (cons 0 (- (length CoordLT) 1))) CoordLT))
	   (vla-put-Elevation FigEntVla 0.0)
         )
         (progn ;->polyline no
            (setq spt (cdr (assoc 10 ed)))
            (if (/= spt nil)(progn
               (setq sptx (car spt) spty (cadr spt))
               (setq spt (list sptx spty ptz))
               (setq ed (subst (cons 10 spt) (assoc 10 ed) ed))
            ))
            (setq ept (cdr (assoc 11 ed)))
            (if (/= ept nil)(progn
               (setq eptx (car ept) epty (cadr ept))
               (setq ept (list eptx epty ptz))
               (setq ed (subst (cons 11 ept) (assoc 11 ed) ed))
            ))
            (setq elept (cdr (assoc 38 ed)))
            (if (/= elept nil) (setq ed (subst (cons 38 0) (assoc 38 ed) ed)) )
            ;dimension start
            (if (= (cdr (assoc 0 ed)) "DIMENSION")(progn
               (setq sp13 (cdr (assoc 13 ed)))
               (setq sp13 (list (car sp13) (cadr sp13) ptz))
               (setq ed (subst (cons 13 sp13) (assoc 13 ed) ed))
               (setq sp14 (cdr (assoc 14 ed)))
               (setq sp14 (list (car sp14) (cadr sp14) ptz))
               (setq ed (subst (cons 14 sp14) (assoc 14 ed) ed))
            ))
            ;dimension end
            (entmod ed)
         )
      );if end
      (setq k (+ k 1))
      (princ (strcat "Coverting Data Num(%) : " (rtos k 2 0) "(" (rtos (* (/ k (float ssn)) 100.0) 2 0) "%)" (chr 13)))
   );repeat end
   (prompt "\nConverting End...")
  
  ;;;Undo 초기화끝
  (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-ACAD-Object)))
  ;;;Undo 초기화끝
  (princ)
)