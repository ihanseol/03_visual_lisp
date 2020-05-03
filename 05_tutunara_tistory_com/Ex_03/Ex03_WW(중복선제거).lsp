;더블 텍스트 포인트 라인 지우는 리습
(defun C:WW() (DoubleEntity_Delete))
(defun DoubleEntity_Delete( / ss ssdup ct len e eb pt lay ang sty hgt str obj obj_list)
 (setq obj_list(list))
 (princ "\nSelect Line objects.") (setq ss (ssget))
  (if ss                                       
   (progn  
;;중복된 엔티티 선택 
    (princ "\nBuilding list of objects.")
    (setq ssdup (ssadd))                      
    (setq len (sslength ss) ct 0)                        
     (while (< ct len)
      (setq e (ssname ss ct))                
      (setq eb (entget e))                   
      (setq ct (+ ct 1)) 
      (setq Name (cdr (assoc 0 eb)))          
       (if (and (/= Name  "LWPOLYLINE")(/= Name  "POLYLINE"))
        (progn
         (cond ((= Name "LINE") (setq pt (cdr (assoc 10 eb)) lay (cdr (assoc 8 eb)))          
          (setq ang (cdr (assoc 11 eb)) sty (cdr (assoc 7 eb)))          
          (setq hgt (cdr (assoc 40 eb))  str (cdr (assoc 1 eb)))
               )
          ( T (setq pt (cdr (assoc 10 eb)) lay (cdr (assoc 8 eb)))          
              (setq ang (cdr (assoc 50 eb)) sty (cdr (assoc 7 eb)))          
              (setq hgt (cdr (assoc 40 eb))  str (cdr (assoc 1 eb)))
          )
         );cond          
               (setq obj (list pt lay ang sty hgt str))
                (if (not (member obj obj_list))        
                 (setq obj_list (cons obj obj_list)) 
                 (ssadd e ssdup)                     
                );if
        );progn
       );if                                      
     );while
     (setq aaa obj_list)
;;삭제될 엔티티 삭제
   (if (> (sslength ssdup) 0)                
    (progn (princ "\nDeleting duplicate objects.")
     (setq len (sslength ssdup) ct 0)                
      (while (< ct len)                      
       (setq e (ssname ssdup ct))          
       (setq ct (+ ct 1))                 
       (entdel e)
      );while
     (princ (strcat "\nDeleted "  (itoa len)  " duplicate objects." ))
    );progn
    (princ "\nNo duplicates found.")       
   );if

  );progn                                            
    (princ "\nNo text objects selected.")
 );if
    (princ)
);defun

