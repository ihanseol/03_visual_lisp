;;; [c]2005 Andrzej Gumula, Katowice, Poland
;;; e-mail: a.gumula@wp.pl
;;; This routine allows hide and display any selected entity.


(defun c:InVis (/ SSet Count Elem)
  
  (defun Dxf (Id Obj)
    (cdr (assoc Id (entget Obj)))
  );end Dxf
  
  (prompt "\nSelect object(s) to hide: ")
  (cond
    ((setq SSet (ssget))
     (repeat (setq Count (sslength SSet))
       (setq Count (1- COunt)
	     Elem (ssname SSet Count))
       (if (/= 4 (logand 4 (Dxf 70 (tblobjname "layer" (Dxf 8 Elem)))))
	 (if (Dxf 60 Elem)
	   (entmod (subst '(60 . 1) (assoc 60 (entget Elem)) (entget Elem)))
	   (entmod (append (entget Elem) (list '(60 . 1))))
	 )
	 (prompt "\nEntity on a locked layer. Cannot hide this entity. ")
       );end if
     );end repeat
    )	 	
  );end cond
  (princ)
);end c:InVis


(defun c:Vis (/ WhatNextSSet Count Elem)

  (defun Dxf (Id Obj)
    (cdr (assoc Id (entget Obj)))
  );end Dxf

 (cond
  ((setq SSet (ssget "_X" '((60 . 1))))
   (initget "Yes No")
   (setq WhatNext (cond
		   ((getkword "\nAll hidden entities will be visible. Continue? No, <Yes>: "))
		   (T "Yes")))
   (cond
   ((= WhatNext "Yes")
    (prompt "\nPlease wait...")
     (repeat (setq Count (sslength SSet))
       (setq Count (1- COunt)
	     Elem (ssname SSet Count))
       (if (/= 4 (logand 4 (Dxf 70 (tblobjname "layer" (Dxf 8 Elem)))))
	 (entmod (subst '(60 . 0) '(60 . 1) (entget Elem)))
	 (prompt "\nEntity on a locked layer. Cannot make visible this entity. ")
       );end if
     );end repeat
    (prompt "\nDone...")
    )
   );end cond
  )
  (T (prompt "\nNo objects was hidden. "))
 );end cond
 (princ)
);end c:Vis

(prompt "\n[c]Loade new commands VIS and INVIS. ")
(prompt "\n[c]2005 Andrzej Gumula. ")
(princ)