(defun c:ts (/ a index b c d e f )
(if(setq a(ssget '((0 . "text"))))
(progn
(setq index 0) 
(repeat (sslength a) 
(setq b(cons(vlax-ename->vla-object(ssname a index))b))
(setq index(1+ index))
);;repeat
(setq c(mapcar '(lambda(x)(vla-get-height x))b))
(while (setq c(vl-remove(setq d(car c))c))
(setq e(cons d e))
);;while 
(setq e(vl-sort(cons d e)'<))
(setq e(apply 'strcat(mapcar '(lambda(x)(strcat(rtos x)(chr 32)))e))) 
(setq f(getdist(strcat "Enter new text height [" e "]:")))
(if f
(progn
(mapcar '(lambda(x)(vla-put-height x f))b)
(princ(strcat "¢º¢º¢º"(itoa(1+ index)) "Text entities changed."))
(princ) 
);;progn 
);;if
);;progn
);;if 
);;defun 
