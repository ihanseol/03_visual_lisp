;**********************************************************************
;바꾸고자 하는 문자의 내용을 원하는 선택한 문자로 자동으로 바꾸는 리습
;
;99.5.13 거제 원에서 재 수정하여 아이콘으로 정리
;*********************************************************************** 
  (defun C:tt ( )
    (prompt "\n원하는 문자를 선택하시오Select text to match --SOURCE--: ")
    (setq e (ssget))
    (setq e (cdr (assoc 1 (entget (ssname e 0)))))
    (prompt "\nSelect text to change --TARGET--: ")
    (setq a (ssget))
    (setq b (sslength a))
    (setq c 0)
    (while (<= 1 b)
           (setq d (ssname a c))
           (if (eq (cdr (assoc 0 (entget d))) "TEXT")
           (entmod (subst (cons 1 e) (assoc 1 (entget d)) (entget d))))
           (setq b (- b 1))
           (setq c (+ c 1))
    )
 )

