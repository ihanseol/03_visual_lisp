(defun C:ART()
       (setvar "cmdecho" 0)
       (SSGET)
       (command "AREA" "E" (SSGET "P"))
       (setq ar (getvar "area"))
       (SETQ AR1 (/ AR 1.0))
       (prompt "AREA (M2) =")(prin1 (SETQ AR2 (RTOS (/ AR1 1000000.0) 2 1)))
       (PROMPT "      AREA (PY) = ")(prin1 (setq ar3 (RTOS (/ ar1 3305800.0) 2 1)))
       (PROMPT "      CLICK TEXT POINT.......")
       (princ)
       (SETQ PO (GETPOINT))
       (COMMAND "STYLE" "STANDARD" "SIMPLEX" "" "" "" "" "" "")(PRIN1)
       (SETQ TESI (* 2.5 (GETVAR "LTSCALE")))
       (SETQ ART1 (STRCAT AR2 " M2"))
       (SETQ ART2 (STRCAT AR3 " PY"))
       (COMMAND "TEXT" "BC" PO tesi "" ART1)(PRIN1)
       (COMMAND "TEXT" "TC" PO TESI "" ART2)(PRIN1)
)





