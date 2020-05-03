(defun C:EQU (/ selobj selobj2 new)
    (setvar "cmdecho" 0)
    (if (setq a (entsel "\nOriginal object select:"))
      (progn (redraw (car a) 3)
        (setq sel (car a)) 
        (setq selobj (entget sel))
        (setq OLDSELECT sel) 

        (princ "\nAlteration object select:")
        (if (setq ss (ssget))
          (progn (setq k 0) 
            (repeat (sslength ss) 
             (setq ent (ssname ss k))
             (setq selobj2 (entget ent))
             (setq #1 (cdr (assoc 0 selobj2)))
             (cond ((= #1 "TEXT")
                   (setq new (assoc 1 selobj))
                   (setq selobj2 (subst new (assoc 1 selobj2) selobj2)))
                   ((= #1 "MTEXT")
                   (setq new (assoc 1 selobj))
                   (setq selobj2 (subst new (assoc 1 selobj2) selobj2)))
                   ((= #1 "DIMENSION")
                   (setq new (assoc 1 selobj))
                   (setq selobj2 (subst new (assoc 1 selobj2) selobj2)))
             ) ;cond
             (entmod selobj2)
             (setq k (1+ k))
            ) ;repeat
           );progn
       );if
      (redraw (car a) 4)
      );progn
     );if
(princ) 
)