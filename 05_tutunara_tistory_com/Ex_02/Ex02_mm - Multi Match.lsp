;다중 특성일치 Multi-Matchprop
;Command : mm

(defun c:mm(/ ss1 ss)
(setvar "cmdecho" 0)
(setq ss1 (entsel "\n ≫ 원본 객체를 선택  : "))
(prompt "\n ≫ 대상 객체를 선택 <다중선택/드래그 가능>...  ")
(setq ss (ssget))
(command "matchprop" ss1 ss "")
(princ)
);defun