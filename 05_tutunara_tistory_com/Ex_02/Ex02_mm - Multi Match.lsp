;���� Ư����ġ Multi-Matchprop
;Command : mm

(defun c:mm(/ ss1 ss)
(setvar "cmdecho" 0)
(setq ss1 (entsel "\n �� ���� ��ü�� ����  : "))
(prompt "\n �� ��� ��ü�� ���� <���߼���/�巡�� ����>...  ")
(setq ss (ssget))
(command "matchprop" ss1 ss "")
(princ)
);defun