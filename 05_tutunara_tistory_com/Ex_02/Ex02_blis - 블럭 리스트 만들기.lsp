; ��Ű��� ��� "�ູ���Ϸ�"
; http://cafe.daum.net/archimore
; �� ����Ʈ �����>> ����/���̸�/����
; 2007.10.02.

;- ���� ũ�⿡ ������� ����ũ��� ǥ�� (����ũ�⿡ �°� Ȯ�� ��ҵ�)
;- ���� : �������� / ��ü���� ����
;- ������ ����
;- ���ȵ� ���� ���ܵ�
;- list�� ũ��� lts ���� �¿��...lts = �ؽ�Ʈ����
;- table�� �����߱� ������.. 2005 ���Ϲ��������� ��밡��

(defun c:blis(/ os lts vctr lt bn-list maxlen k n bnum-list bed bn len op1 invbn ss
               num op2 op3 @bn @num ss1 minpt maxpt hor-dis ver-dis dis sel)
;->*error* start   
 (defun *error* (msg)(princ "error: ")(princ msg)
 (setvar "osmode" os)
 (princ))
;-<*error* end
 (vl-load-com)
 (setvar "cmdecho" 0)
 (prompt "\n>>�� ����Ʈ �����>> ����/���̸�/����")
 (setq os (getvar "osmode")
       lts (getvar "ltscale")
       vctr (getvar "viewctr")) 
 (setq lt (* lts 1)) 
 (setq bn-list '() maxlen 0 k 0 n 0 bnum-list '() sel nil)
 (setq bed (tblnext "block" t))
 (while bed
  (setq bn (cdr (assoc 2 bed)))
  (if (/= (substr bn 1 1) "*")
   (progn
    (setq len (car (cadr (textbox (list (cons 1 bn) (cons 40 lt))))))
    (setq maxlen (max maxlen len))
    (setq bn-list (append bn-list (list bn)))
   );progn
  );if
  (setq bed (tblnext "block"))
 );while
 (setq sel (getint "\n1.��������<1> / 2.��ü<����>"))
 (if (= sel 1) 
  (progn
   (setq p1 (getpoint "\nFirst point:")
         p2 (getcorner p1 "->Second point:"))
  ) ;progn
 ) ;if
 (setq op1 (getpoint "\n������ġ�� Ŭ�� ���ּ���:"))
 (command "undo" "be")
 (setvar "osmode" 0)
 (repeat (length bn-list)
  (setq invbn (nth n bn-list))
  (if (= sel 1)
   (setq ss (ssget "c" p1 p2 (list (cons 0 "insert") (cons 2 invbn))))
   (setq ss (ssget "x" (list (cons 0 "insert") (cons 2 invbn))))
  );if
  (if (/= ss nil) 
   (progn 
    (setq num (sslength ss))
    (setq bnum-list (append bnum-list (list (cons invbn num))))
   ) ;progn
  ) ;if
  (setq n (1+ n))
 );repeat
 (setq op2 (polar op1 0 (* lt 5)))
 (setq op3 (polar op2 0 (* maxlen 1.2)))
 (repeat (length bnum-list)
  (setq @bn (car (nth k bnum-list))
        @num (cdr (nth k bnum-list)))
  (setq ss1 (ssadd))
  (command "insert" @bn vctr 1 "" 0) (ssadd (entlast) ss1) 
  (vla-GetBoundingBox (vlax-ename->vla-object (entlast)) 'MinPt 'MaxPt) 
  (setq MinPt (vlax-safearray->list MinPt)) 
  (setq MaxPt (vlax-safearray->list MaxPt))  
  ;(command "pline" minpt (list (car maxpt) (cadr minpt)) maxpt (list (car minpt) (cadr maxpt)) "c")
  ;(ssadd (entlast) ss1)
  (setq hor-dis (distance minpt (list (car maxpt) (cadr minpt))))
  (setq ver-dis (distance minpt (list (car minpt) (cadr maxpt))))
  (if (>= hor-dis ver-dis) (setq dis hor-dis) (setq dis ver-dis)) 
  (if (or (> dis (* lt 3)) (< dis (* lt 3))) (command "scale" ss1 "" minpt "r" dis (* lt 3)))
  (command "move" ss1 "" minpt (polar op1 (+ (/ pi 2) pi) (* (/ ver-dis 2) (/ (* lt 3) dis))))
  (command "text" (polar op2 (+ (/ pi 2) pi) (/ lt 2))  lt 0 @bn)
  (command "text" (polar op3 (+ (/ pi 2) pi) (/ lt 2)) lt 0 @num)
  (setq op1 (polar op1 (+ (/ pi 2) pi) (* lt 4)))
  (setq op2 (polar op2 (+ (/ pi 2) pi) (* lt 4)))
  (setq op3 (polar op3 (+ (/ pi 2) pi) (* lt 4)))
  (setq k (1+ k))
 );repeat
(command "undo" "e")
(setvar "osmode" os)
(princ)
);defun

