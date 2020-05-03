(defun c:cbt (/)
  (if (dictremove (namedobjdict) "acad_dgnlinestylecomp")
    (progn
      (princ
 "\nClean DGN-rubbish complete.  ASAP, Audit & purge is recommended !!! ."
      )
    )
    (princ
      "\nacad_dgnlinestylecomp not found.  This file is virgin !!! ."
    )
  )
  (princ)
)
