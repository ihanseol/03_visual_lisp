;�±վ�~�߸Ա��߻���~!

(defun c:laa(/ la ln co lco colist n la1 la2 @1) 
(setvar "cmdecho" 0) 
(prompt "\n>>���̾� ����Ʋ���� ����/�ڵ� �����ϱ�..") 
(setq la (getvar "clayer")) 
(setq ln nil co nil lco nil colist nil n 0) 
(while (= ln nil)  
  (setq ln (getstring "\n>> Layer name :")) 
  (if (or (/= (tblsearch "layer" ln) nil) (= ln ""))  
    (progn (princ "->�ߺ��� �̸��� �ְų� �̸��� �������� �ʾҽ��ϴ�") (setq ln nil)) 
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
(setq @1 (getint "\n���̾���� �ڵ�����<����> / ��������<1> :")) 
(cond ((= @1 1)  
        (while (= lco nil) 
         (setq lco (acad_colordlg 256)) 
         (foreach x colist (progn (if (= x lco) (setq lco nil)))) 
         (if (or (<= lco 0) (>= lco 256) (= lco nil))  
          (progn (princ "->�������� �ʴ� ���� �Ǵ� �ߺ��� �����Դϴ�") (setq lco nil)) 
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