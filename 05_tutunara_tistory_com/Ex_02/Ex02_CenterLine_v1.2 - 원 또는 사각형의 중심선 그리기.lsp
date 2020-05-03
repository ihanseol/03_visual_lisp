;;=====================================================================
;  �߽ɼ� �׸���
;    * �� �Ǵ� ��ũ�� �߽ɼ� �׸���
;    * �簢��(�������� ����)�� �߽ɼ� �׸���
;  
;  Modify : - �߽ɼ� ���� ���� ���� ��� �߰� by nadau (2009.08.03)
;


(defun c:cc(/ os ss k ent elist plist VtxList cenp rad po1 po2 po3 po4 dam ext la cenx ceny dix diy plusc plusx plusy paa)

  (setvar "cmdecho" 0)   
  (setq os (getvar "osmode") )  ; Current Osnap Setting Save   
  (prompt "\n ** Draw Center Line for Circle or Arc or Rectangle(Polyline) ** \n")
   
  (setq ss (ssget '((0 . "circle,arc,LWPOLYLINE"))))
  
  (if (/= ss nil) (progn
  
  (setq paa (getint "\nInput length to extend the center line <10>: " ))
  (if (= paa nil) (setq paa 10))
  (if (and (>= paa 0) (<= paa 100) ) (progn


  (setq k 0)
  (setvar "osmode" 0) 
  (command "undo" "be") 

  (repeat (sslength ss)
   (setq ent (ssname ss k))
   (setq elist (entget ent))  
   
   (if (/= (cdr (assoc 0 elist)) "LWPOLYLINE") (progn
   
   (setq cenp (cdr (assoc 10 elist))) 
   (setq rad (cdr (assoc 40 elist)))
   (setq dam (* 2 rad)
         plusc (* (/ dam 2) (/ paa 100.0) )
         ext (+ plusc rad)
   ); end setq
   
   (setq po1 (polar cenp pi ext) 
         po2 (polar cenp 0 ext)    
         po3 (polar cenp (/ pi 2) ext) 
         po4 (polar cenp (+ (/ pi 2) pi) ext) 
   ); end setq 
   
  ); end progn
  
  (progn
      (setq VtxList (GetPolyVtx elist)
          plist (getxy VtxList)
          cenx (/ (+ xmin xmax) 2)
          ceny (/ (+ ymin ymax) 2)
          dix (/ (- xmax xmin) 2)
          diy (/ (- ymax ymin) 2)
          cen (list cenx ceny)
          ); end setq
    (setq plusx (* dix (/ paa 100.0))
          plusy (* diy (/ paa 100.0)))
    (setq po1 (polar cen pi (+ dix plusx))
          po2 (polar cen 0 (+ dix plusx))
          po3 (polar cen (+ (/ pi 2) pi) (+ diy plusy))
          po4 (polar cen (/ pi 2) (+ diy plusy)))
  ); end progn
  
  ); end if
  
   (DrawCenterLine po1 po2 po3 po4)
   (setq k (+ 1 k)) 
  );repeat 

   (command "undo" "e")
   (setvar "osmode" os)
  ); end progn
  
  (progn (prompt "\n�Է� ���� ������ [0 ~ 100]�Դϴ�.\n")
  );end progn
  ); end if
  ); end progn
  ); end if
  (princ)
) ;defun end


;
; [[[ SubRoutine ]]]

(defun DrawCenterLine(p1 p2 p3 p4 / cla)
   (setq cla (getvar "clayer"))  ; Current Layer Setting Save
   (command "layer" "s" "1" "")   ;  <-�߽ɼ� ���̾�� �Ͻ��� ���� (�̸� ���� �ʿ�)
   (command "line" p1 p2 "")
   (command "line" p3 p4 "")
   (setvar "clayer" cla) ; ����ߴ� ���̾�� �ǵ���    
)


(defun getxy(vtxlist) ; �Ƕ����ǰ� ����Ʈ�� ��ǥ����
   (setq p1 (car vtxlist)
         p2 (cadr vtxlist)
         p3 (caddr vtxlist)
            p4 (cadddr vtxlist))
   (setq 1x (car p1) 1y (cadr p1)
         2x (car p2) 2y (cadr p2)
         3x (car p3) 3y (cadr p3)
         4x (car p4) 4y (cadr p4))
   (setq xmax (max 1x 2x 3x 4x)
         xmin (min 1x 2x 3x 4x))
   (setq ymax (max 1y 2y 3y 4y)
         ymin (min 1y 2y 3y 4y))
   (setq pa (list xmin ymin) ; ���� �ϴ���
         pb (list xmax ymin) ; ������ �ϴ���
         pc (list xmin ymax) ; ���� ���
         pd (list xmax ymax)) ; ���� ���
)

(defun GetPolyVtx(EntList)
  (setq VtxList '())
  (foreach x EntList
   (if (= (car x) 10)
    (setq VtxList (append VtxList (list (cdr x))))
   )
  ) VtxList
)

;;
;; [[[ End SubRoutine ]]]
