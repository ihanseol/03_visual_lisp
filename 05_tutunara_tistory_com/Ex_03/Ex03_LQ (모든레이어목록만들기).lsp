;;=================================================================
;  레이어의 목록을 만드는 리습
;  ->모든 레이어의 목록을 만듬
;;-----------------------------------------------------------------
(defun c:lq(/ os cl la1 la2 la-lst lengnum d1 dis1 ts1 cc lt ln1 n
              pt1 pt2 pt3 pt4 pt5)
   (setq os (getvar "osmode"))
   (setq cl (getvar "clayer"))
   (setvar "osmode" 0)
   (command "layer" "on" "*" "t" "*" "")
   (setq la1 (tblnext "layer" t))
   (setq la1 (cdr (cadr la1)));layer name
   (setq la-lst (list la1))
   (setq la2 (tblnext "layer"))
   (while la2
       (setq la2 (cdr (cadr la2)));layer name
       (if (/= (wcmatch la2 "*|*") T) (setq la-lst (cons la2 la-lst)) )
       (setq la2 (tblnext "layer"))
   )
   (setq la-lst (vl-sort la-lst '<))
   (setq lengnum (length la-lst))
   (setq pt1 (getpoint "\n정렬 상단점->"))
   (setq d1 (getdist pt1 "\정렬 하단점->"))
   (setq dis1 (/ d1 lengnum))
   (setq ts1 (/ dis1 2))
   (setq n 0)
   (repeat lengnum
      (setq pt2 (polar pt1 (* 1.5 pi) dis1))
      (setq pt3 (polar pt2 0 (* dis1 3)))
      (setq pt4 (polar pt3 0 (* dis1 6)))
      (setq pt5 (polar pt4 0 (* dis1 3)))
      (setq ln1 (nth n la-lst))
      (setq cc (cdr (nth 3 (tblsearch "layer" ln1))))
      (setq lt (cdr (nth 4 (tblsearch "layer" ln1))))
      (setvar "clayer" ln1)
      (command "line" pt2 pt3 "")
      (command "text" pt3 ts1 0 lt)
      (command "text" pt4 ts1 0 cc)
      (command "text" pt5 ts1 0 ln1)
      (setq pt1 pt2)
      (setq n (1+ n))
   )
   (setvar "osmode" os)
   (setvar "clayer" cl)
   (princ)
)