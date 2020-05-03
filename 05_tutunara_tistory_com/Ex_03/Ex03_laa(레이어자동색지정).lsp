;태균아~잘먹구잘살자~!

(defun c:laa(/ la ln co lco colist n la1 la2 @1) 
(setvar "cmdecho" 0) 
(prompt "\n>>레이어 색상틀리게 수동/자동 생성하기..") 
(setq la (getvar "clayer")) 
(setq ln nil co nil lco nil colist nil n 0) 
(while (= ln nil)  
  (setq ln (getstring "\n>> Layer name :")) 
  (if (or (/= (tblsearch "layer" ln) nil) (= ln ""))  
    (progn (princ "->중복된 이름이 있거나 이름이 지정되지 않았습니다") (setq ln nil)) 
  );if 
);while 
(setq la1 (tblnext "layer" t)) 
(setq co (cdr (assoc 62 la1))) 
(setq colist (list co)) 
(setq la2 (tblnext "layer")) 
(while la2 
  (setq co (list (cdr (assoc 62 la2)))) 
  (setq colist (append colist co)) 
  (setq la2 (tblnext "layer")) 
);while 
(setq @1 (getint "\n레이어색상 자동지정<엔터> / 직접지정<1> :")) 
(cond ((= @1 1)  
        (while (= lco nil) 
         (setq lco (acad_colordlg 256)) 
         (foreach x colist (progn (if (= x lco) (setq lco nil)))) 
         (if (or (<= lco 0) (>= lco 256) (= lco nil))  
          (progn (princ "->지원하지 않는 색상 또는 중복된 색상입니다") (setq lco nil)) 
         );if       
        );while 
       ) 
       (t 
        (while (= lco nil) 
         (setq n (1+ n)) 
         (setq lco n) 
         (foreach x colist (progn (if (= x n) (setq lco nil)))) 
         );while  
       ) 
  );cond 
(command "layer" "m" ln "c" lco "" "") 
(setvar "clayer" la) 
(princ) 
);defun 