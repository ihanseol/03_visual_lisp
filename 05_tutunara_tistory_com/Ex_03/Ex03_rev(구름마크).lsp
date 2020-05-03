;;=================================================
;  revcloud 구름마크
;--------------------------------------------------
(defun c:rev(/ myerror os unts bulge divide remaind
                sp pts ip ai p1 p2 p3 ang sub i)
 ;start --- Internal error handler ----------
  (defun myerror(S)
  (if (/= s "Function cancelled")(princ (strcat "\nError:" s)))
  (setvar "osmode" os) (command "redraw")
  (setq *error* olderr)(princ) )
  (setq olderr *error* *error* myerror)
 ;end----------------------------------------
  (setq os (getvar "osmode")) (setvar "osmode" 0)
  (setq dsc (getvar "dimscale"))
  (setq unts (* dsc 15.0)   bulge   0.6
        divide  0.4         remaind 0.5
  )
  (if (setq sp (getpoint "\nFirst point: "))
    (progn
      (setq pts (list sp)  ip sp)
      (while ip
        (initget 32 (if (<= 3 (length pts)) "Close Undo" "Undo"))
        (setq ip  (getpoint "\nTo point: " ip))
        (cond ((null ip))   ;pts -> min 1 ea
              ((= ip "Undo") (setq ip (cadr pts))
                             (if ip (grdraw ip (car pts) 0))
                             (setq pts (cdr pts))
              )
              ((= ip "Close") (grdraw (car pts) sp 7)
                              (setq pts (cons sp pts)  ip nil)
              )
              (t (grdraw ip (car pts) 7)
                 (setq pts (cons ip pts))
              )
        )
      )
      (setq ai 0)
      (repeat (- (length pts) 2)
        (setq p1 (car pts)
              p2 (cadr pts)
              p3 (caddr pts)
              ap (+ (angle (list 0.0 0.0 0.0) (list 0.0 1.0 0.0)) (angle p1 p2))
        )
        (command "ucs" "3p" p1 p2 (polar p1 ap 1))
        (setq ang (angle (trans p2  0 1) (trans p3 0 1))
              sub (- ang pi)
              ai (+ ai (if (< 0 sub) (- sub) ang))
        )
        (command "ucs" "")
      )
      (if (> 0 ai) (setq pts (reverse pts)))
      (command "pline" (car pts) "a")
      (setq i -1)
      (repeat (1- (length pts))
         (cloud-sub (nth (setq i (1+ i)) pts) (nth (1+ i) pts))
      )
      (command "" )
      (setq i -1)
      (repeat (1- (length pts))
        (grdraw (nth (1+ i) pts) (nth (+ i 2) pts) 0)
        (setq i (1+ i))
      )
    )
  )
  (setvar "attdia" 0) (command "redraw") (setvar "osmode" os)
  (princ)
)
;;SUB PRG.
(defun cloud-sub (sp ep / ang am len ea dist dist1
                          dist2 dst1 dst2 p3 p4 p5 p6 p7)
  (setq ang (angle sp ep)
        am (+ ang (angle (list 0.0 0.0 0.0) (list 0.0 -1.0 0.0)))
        len (distance sp ep)
        ea  (fix (+ remaind (/ len unts )))
  )
  (if (zerop ea) (setq ea 1))
  (setq dist (/ len ea)
        dist1 (* divide dist)   dst1 (* 0.5 dist1)
        dist2 (- dist dist1)    dst2 (* 0.5 dist2)
        p3 sp
  )
  (repeat ea
    (setq p4 (polar (polar p3 ang dst1) am (* bulge dst1))
          p5 (polar p3 ang dist1)
          p6 (polar (polar p5 ang dst2) am (* bulge dst2))
          p7 (polar p5 ang dist2) )
    (command "s" p4 p5 "s" p6 (setq p3 p7))
  )
)





