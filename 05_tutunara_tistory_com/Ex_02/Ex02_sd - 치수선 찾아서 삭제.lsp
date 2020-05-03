;; ** ���� ���� ������ ġ�������� ã�Ƽ� �����մϴ�. **
;;

(defun c:sd (/ #a #b #c #index)
  (setq #a(ssget '((0 . "dimension,leader,mtext,text"))))
  (setq #index 0)
  
  (repeat (sslength #a)
    (setq #b(ssname #a #index))

    (if (eq "LEADER" (strcase(cdr(assoc 0(entget #b)))))

      (progn
      (setq #c(cdr(assoc 340(entget #b))))
      (mapcar 'entdel (list #b #c))
      );;progn

    (entdel #b)

    );;if

    (setq #index(1+ #index))
  );;repeat

);;defun