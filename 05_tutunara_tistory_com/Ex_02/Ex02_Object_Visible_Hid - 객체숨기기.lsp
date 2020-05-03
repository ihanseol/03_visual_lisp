;; 선택 객체 보이기 / 숨기기 - 레이어 무관

(defun c:uv (/ SSet Count Elem) ; 숨기기
  
  (defun Dxf (Id Obj)
    (cdr (assoc Id (entget Obj)))
  );end Dxf
  
  (prompt "\nSelect object(s) to hide: ") ;숨길 객체를 선택하시오  

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
  (prompt "\nEntity on a locked layer. Cannot hide this entity. ") ;이 객체는 잠긴 레이어이므로 숨길 수 없습니다
       );end if
     );end repeat
    )   
  );end cond
  (princ)
);end c:InVis

;━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun c:ve (/ WhatNextSSet Count Elem)  ; 보이기

  (defun Dxf (Id Obj)
    (cdr (assoc Id (entget Obj)))
  );end Dxf

 (cond
  ((setq SSet (ssget "_X" '((60 . 1))))
   (initget "Yes No")
   (setq WhatNext (cond
     ((getkword "\nAll hidden entities will be visible. Continue? No, <Yes>: ")) ;숨겨진 모든 객체가 보여집니다. 계속하시겠습니까?
     (T "Yes")))
   (cond
   ((= WhatNext "Yes")
    (prompt "\nPlease wait...") ;기다려 주세요...
     (repeat (setq Count (sslength SSet))
       (setq Count (1- COunt)
      Elem (ssname SSet Count))
       (if (/= 4 (logand 4 (Dxf 70 (tblobjname "layer" (Dxf 8 Elem)))))
  (entmod (subst '(60 . 0) '(60 . 1) (entget Elem)))
  (prompt "\nEntity on a locked layer. Cannot make visible this entity. ") ; 이 객체는 잠긴 레이어이므로 보이게 할 수 없습니다 !!
       );end if
     );end repeat
    (prompt "\nDone...") ; 실행완료
    )
   );end cond
  )
  (T (prompt "\nNo objects was hidden. ")) ; 객체가 숨겨지지 않았습니다
 );end cond
 (princ)
);end c:Vis

;(prompt "\nCommands VIS[보임] and INVIS[숨김] ")
(princ)
