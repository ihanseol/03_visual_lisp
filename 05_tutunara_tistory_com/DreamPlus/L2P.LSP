(defun c:l2p ()
     (prompt "\nSeletting .........")
      (setq xx1 (ssget "x" '((0 . "LINE"))))
      (setq num (sslength xx1))
      (setq nun nun)
      (setq index 0)
 (repeat num
	  (setq entl (entget (ssname xx1 index)))
	  (setq index (1+ index))

	  (setq inddd (+ index 5))
	  (if (= (rem inddd 4) 1)
	      (setq zz " \\ ")
	      (if (= (rem inddd 4) 2)
		  (setq zz " \| ")
		  (if (= (rem inddd 4) 3)
		       (setq zz " \/ ")
	      (setq zz " \- ")
	  )))

	  (setq ass (assoc 0 entl))

  (if (= "LINE" (cdr ass))
     (progn
     (setq se1 (car entl))
     (setq se2 (cdr se1))
     (command "pedit" se2 "y" "")
(prompt "\r") (prompt zz) (prompt " Line to Pline .......") (princ index) (prompt "/") (princ num)
     )
  )
 )
)

