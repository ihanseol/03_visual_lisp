;;;SCALEAXIS.LSP

;--------------------------------------------------
; ERROR TRAPPING
;--------------------------------------------------

(defun errtrap (msg)
  (cond
    ((not msg))
    (
      (member msg '("Function cancelled" "quit / exit abort"))
      (command "undo" "")
    )
    (
      (princ (strcat "\nError: " msg))
      (command "undo" "")
    )
  );cond
);defun

;--------------------------------------------------
;  MAIN ROUTINE
;--------------------------------------------------

(defun c:scaleaxis (/ *error* *ss1 bspt ax mult refpt refdx newdx)

(command "._undo" "end" "._undo" "begin")
(setq *error* errtrap)

(setq ss1 (ssget))
(setq bspt (getpoint "\nSelect basepoint: "))
(initget "X Y Z")
(if
  (not
    (setq ax (getkword "\nSpecify axis to scale: <X> "))
  );not
  (setq ax "X")
);if

(if
  (not 
    (setq mult (getreal "\nEnter scale factor or <Reference>: "))
  );not
  (progn
    (setq refpt1 (getpoint "\nSpecify reference length: "))
    (setq refdx (getdist refpt1 "\nSpecify second point: "))
    (setq newdx (getdist refpt1 "\nSpecify new length: "))
    (setq mult (/ newdx refdx))
  );progn
);if

(setvar "expert" 2)
(setvar "explmode" 1)
(command "._-block" "SCALETEMP" bspt ss1 "")
(command "._-insert" "SCALETEMP" ax mult bspt "0")
(command "._explode" "last" "")
(command "._-purge" "blocks" "SCALETEMP" "n")
(setvar "expert" 1)

(command "._undo" "end")

(princ)
(*error* nil)
)