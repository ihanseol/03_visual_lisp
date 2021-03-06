(defun c:plw (/ ss spw epw index ent count old new)
  (setq ss (ssget (list (cons 0 "lwpolyline"))))
  (if ss
    (progn
      (setq spw (getreal "\n ������ ���� : "))
      (if (not spw) (setq spw 0))
      (setq epw (getreal "\n ���� ���� : "))
      (if (not epw) (setq epw 0))
      (setq index 0)
      (repeat (sslength ss)
        (setq ent (entget (ssname ss index)))
        (setq count 0)
        (repeat (length ent)
          (setq old (nth count ent))
          (cond
            ((= (car old) 40)
              (setq new (cons 40 spw))
              (setq ent (subst new old ent))
            )
            ((= (car old) 41)
              (setq new (cons 41 epw))
              (setq ent (subst new old ent))
            )
            ((= (car old) 43)
              (setq ent (append (reverse (cdr (member old (reverse ent)))) (cdr (member old ent))))
            )
          )
          (setq count (1+ count))
        )
        (entmod ent)
        (entupd (ssname ss index))
        (setq index (1+ index))
      )
    )
  )
  (princ)
)
