;; ���� ��ü ���̱� / ����� - ���̾� ����

(defun c:uv (/ SSet Count Elem) ; �����
  
  (defun Dxf (Id Obj)
    (cdr (assoc Id (entget Obj)))
  );end Dxf
  
  (prompt "\nSelect object(s) to hide: ") ;���� ��ü�� �����Ͻÿ�  

(cond
    ((setq SSet (ssget))
     (repeat (setq Count (sslength SSet))
       (setq Count (1- COunt)
      Elem (ssname SSet Count))
       (if (/= 4 (logand 4 (Dxf 70 (tblobjname "layer" (Dxf 8 Elem)))))
  (if (Dxf 60 Elem)
    (entmod (subst '(60 . 1) (assoc 60 (entget Elem)) (entget Elem)))
    (entmod (append (entget Elem) (list '(60 . 1))))
  )
  (prompt "\nEntity on a locked layer. Cannot hide this entity. ") ;�� ��ü�� ��� ���̾��̹Ƿ� ���� �� �����ϴ�
       );end if
     );end repeat
    )   
  );end cond
  (princ)
);end c:InVis

;������������������������������������������������������������������������������������������

(defun c:ve (/ WhatNextSSet Count Elem)  ; ���̱�

  (defun Dxf (Id Obj)
    (cdr (assoc Id (entget Obj)))
  );end Dxf

 (cond
  ((setq SSet (ssget "_X" '((60 . 1))))
   (initget "Yes No")
   (setq WhatNext (cond
     ((getkword "\nAll hidden entities will be visible. Continue? No, <Yes>: ")) ;������ ��� ��ü�� �������ϴ�. ����Ͻðڽ��ϱ�?
     (T "Yes")))
   (cond
   ((= WhatNext "Yes")
    (prompt "\nPlease wait...") ;��ٷ� �ּ���...
     (repeat (setq Count (sslength SSet))
       (setq Count (1- COunt)
      Elem (ssname SSet Count))
       (if (/= 4 (logand 4 (Dxf 70 (tblobjname "layer" (Dxf 8 Elem)))))
  (entmod (subst '(60 . 0) '(60 . 1) (entget Elem)))
  (prompt "\nEntity on a locked layer. Cannot make visible this entity. ") ; �� ��ü�� ��� ���̾��̹Ƿ� ���̰� �� �� �����ϴ� !!
       );end if
     );end repeat
    (prompt "\nDone...") ; ����Ϸ�
    )
   );end cond
  )
  (T (prompt "\nNo objects was hidden. ")) ; ��ü�� �������� �ʾҽ��ϴ�
 );end cond
 (princ)
);end c:Vis

;(prompt "\nCommands VIS[����] and INVIS[����] ")
(princ)
