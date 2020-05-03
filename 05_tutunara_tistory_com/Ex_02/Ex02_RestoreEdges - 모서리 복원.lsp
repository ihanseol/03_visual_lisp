;;�̹� Chamfer, Fillet �Ǿ� �ִ� ���� ������ �� �츮��
;; Restore to the edges 

(defun c:ree (/ os col 1st 2nd pt p1 p2) 

 (defun *error* (msg)
    (princ "error: ")(princ msg)
    (setvar "osmode" os)    
    (setvar "cecolor" col)  
    (setvar "clayer" cla)      
   (princ)
  ) ;defun

    (prompt "\n ** << �����ϰ��� �ϴ� Line 2���� ���� �ϼ��� >> **") 

    (setq os (getvar "osmode")) 
    (setq cla (getvar "clayer")) 
    (setq col (getvar "cecolor")) 
    (setvar "osmode" 0) 

(while   (setq 1st (entsel)) 
(if 1st (setq c_layer (cdr (assoc 8 (entget (car 1st))))))
          (setq 2nd (entsel))
          (setq p1 (osnap (cadr 1st) "near")
                    p2 (osnap (cadr 2nd) "near") ) 
           (setq 1st (osnap (cadr 1st) "endp") 
                   2nd (osnap (cadr 2nd) "endp") )
          (setq pt (inters p1 1st p2 2nd nil)) 

      (command "layer" "m" "1"  "") ; 1 <- Layer Setting

     (command "line" 1st pt 2nd "") )
     (setvar "osmode" os) 
     (setvar "cecolor" col) 
     (setvar "clayer" cla)
     (princ)

) ;defun end