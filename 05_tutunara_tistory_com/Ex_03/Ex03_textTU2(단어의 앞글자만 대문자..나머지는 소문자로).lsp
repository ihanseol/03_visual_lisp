(defun c:tu2()(texts-first-upcase))

(defun text-first-upcase (strs / newstrs )
  (setq newstrs (strcase (substr strs 1 1))
        strs (substr strs 2)
  )        
  (while (/= "" strs)
    (if (wcmatch (substr strs 1 2) " ?")
      (setq newstrs (strcat newstrs (strcase (substr strs 1 2)))
            strs    (substr strs 3)
      )            
      (setq newstrs (strcat newstrs (strcase (substr strs 1 1) t))
            strs    (substr strs 2)
      )
    )
  )
  newstrs
)

(defun texts-first-upcase (  / ss n t-ent)
  (setq ss (ssget '((0 . "text")))
        n 0
  )        
  (if ss
    (progn
      (repeat (sslength ss)
        (setq t-ent (entget (ssname ss n)))
        (entmod (subst (cons 1 (text-first-upcase (cdr (assoc 1 t-ent)))) (assoc 1 t-ent) t-ent))
        (setq n (1+ n))
      )
    )
  )
)




