(defun C:PEJA (/ peac cmde); = Polyline Edit: Join All
  (setq peac (getvar 'peditaccept))
  (setvar 'peditaccept 1)
  (setq cmde (getvar 'cmdecho))
  (setvar 'cmdecho 0)
  (command "_.pedit" pause "_join" "_all" "" "")
  (setvar 'peditaccept peac)
  (setvar 'cmdecho cmde)
  (princ)
)
 


 