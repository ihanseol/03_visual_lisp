(defun C:DELNLF (/ %1 %2)
(if
(setq %1
(member '(102 . "{ACAD_XDICTIONARY")
(entget (cdr (assoc 330 (entget (tblobjname "LAYER" "0")))))
)
)
(if
(setq %2
(member '(3 . "ACAD_LAYERFILTERS")  
(entget (cdr (assoc 360 %1)))
)
)
(dictremove (cdr (assoc 360 %1)) "ACAD_LAYERFILTERS")
)
Nil
)
)
