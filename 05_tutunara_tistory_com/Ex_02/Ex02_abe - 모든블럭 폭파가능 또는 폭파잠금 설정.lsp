;marko_ribar

; ���鳻�� ���� ���� �������� ����
(defun c:abeY ( / ss ssn k ent BlockName)
(setq ss (ssget "X" '((0 . "INSERT")) ))
(setq ssn (sslength ss))
(setq k -1)
(repeat ssn
(setq k (1+ k))
(setq ent (ssname ss k))
(setq BlockName (cdr (assoc 2 (entget ent))))
(BlkSetExplodability BlockName 1)
)
)

;; ���鳻�� ���� ���� �Ұ������� ����
(defun c:abeN ( / ss ssn k ent BlockName)
(setq ss (ssget "X" '((0 . "INSERT")) ))
(setq ssn (sslength ss))
(setq k -1)
(repeat ssn
(setq k (1+ k))
(setq ent (ssname ss k))
(setq BlockName (cdr (assoc 2 (entget ent))))
(BlkSetExplodability BlockName 0)
)
)
(defun BlkSetExplodability (BlockName Explodability / e ed e330 e330d)
;second argument 0=non-Explodability 1=Explodability
   (setq e (tblobjname "block" BlockName)
         ed (entget e)
         e330 (cdr (assoc 330 ed))
         e330d (entget e330)
         e330d (subst (cons 280 Explodability) (assoc 280 e330d) e330d)
   );setq
   (entmod e330d)
   (prin1)
)
(princ "\nTo set all blocks explodablility to be explodable type \"abeY\" ")
(princ "\nTo set all blocks explodablility to be nonexplodable type \"abeN\" ")
(princ)