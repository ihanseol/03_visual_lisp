; 아키모아 운영진 "행복한하루"
; http://cafe.daum.net/archimore
; 텍스트 표식넣기
; 2007.06.08

(defun c:cct(/ en elist @p1 @p2 @x @y dx dy ag1 al op1 p1 p2 p3 p4 rad1 rad2 dxx dyy dis #1 #2)
   (setvar "cmdecho" 0)
   (command "undo" "be")
   (setq os (getvar "osmode"))
   (setvar "osmode" 0)   
   (prompt "\n>>텍스트 표식그리기>>")
   (setq ss (ssget '((0 . "text"))))
   (command "justifytext" ss "" "mc")
   (setq n (sslength ss))
   (setq k 0)

   (setq a "\n표식선택: 직원<1>/원<2>/타원<3>/마름모꼴<4>/삼각<5>/직6각<6>/6각<7>/사각<엔터>:")
   (initget "1 2 3 4 5 6 7 8")
   (setq kw (getkword a))
;--------------------------------------직원 형
   (cond ((= kw "1")
         (repeat n
          (setq en (ssname ss k))
          (setq elist (entget en))
          (setq @p1 (car (textbox elist)))
          (setq @p2 (cadr (textbox elist)))
          (setq @x (car @p2) @y (cadr @p2) )
          (setq dy (/ @y 4) dx (/ @y 2));->간격조정
          (setq ag1 (cdr (assoc 50 elist)))
          (setq al (cdr (assoc 11 elist)))

          (setq op1 (polar al (- ag1 (/ pi 2)) (+ dy (/ @y 2))))
          (setq p1 (polar op1 (- ag1 pi) (+ dx (/ @x 2))))
          (setq p2 (polar p1 ag1 (+ @x (* dx 2))))
          (setq p3 (polar p2 (+ ag1 (/ pi 2)) (+ @y (* dy 2))))
          (setq p4 (polar p3 (+ ag1 pi) (+ @x (* dx 2))))

          (command "pline" p1 p2 "a" p3 "l" p4 "a" p1 "")
          (setq k (1+ k))
         ) ;repeat
         )
;--------------------------------------원형
         ((= kw "2")
         (repeat n
          (setq en (ssname ss k))
          (setq elist (entget en))
          (setq @p1 (car (textbox elist)))
          (setq @p2 (cadr (textbox elist)))
          (setq @x (car @p2) @y (cadr @p2) )
          (setq dy (/ @y 4) dx (/ @y 2));->간격조정
          (setq ag1 (cdr (assoc 50 elist)))
          (setq al (cdr (assoc 11 elist)))

          (setq op1 (polar al (- ag1 (/ pi 2)) (+ dy (/ @y 2))))
          (setq p1 (polar op1 (- ag1 pi) (+ dx (/ @x 2))))
          (setq rad (distance p1 op1))
          (command "circle" al rad)
          (setq k (1+ k))
         ) ;repeat
         )
 
;--------------------------------------타원
         ((= kw "3")
          (repeat n
          (setq en (ssname ss k))
          (setq elist (entget en))
          (setq @p1 (car (textbox elist)))
          (setq @p2 (cadr (textbox elist)))
          (setq @x (car @p2) @y (cadr @p2) )
          (setq dy (/ @y 4) dx (/ @y 2));->간격조정
          (setq ag1 (cdr (assoc 50 elist)))
          (setq al (cdr (assoc 11 elist)))

          (setq rad1 (polar al (+ ag1 pi) (+ (/ @x 2) dx)))
          (setq rad2 (polar al (+ ag1 (/ pi 2)) (+ (/ @x 5) dx)))

          (command "ellipse" "c" al rad1 rad2)
          (setq k (1+ k))
         ) ;repeat
         )
;--------------------------------------마름모꼴
         ((= kw "4")
          (repeat n
          (setq en (ssname ss k))
          (setq elist (entget en))
          (setq @p1 (car (textbox elist)))
          (setq @p2 (cadr (textbox elist)))
          (setq @x (car @p2) @y (cadr @p2) )
          (setq dy (/ @y 4) dx (/ @y 2));->간격조정
          (setq ag1 (cdr (assoc 50 elist)))
          (setq al (cdr (assoc 11 elist)))
 
          (setq dxx (+ dx (/ @x 2)))
          (setq dyy (+ dx (/ @y 2)))          
 
          (setq p1 (polar al (+ ag1 pi) (* dxx 1.5) ))
          (setq p2 (polar al ag1 (* dxx 1.5) ))
          (setq p3 (polar al (+ ag1 (/ pi 2)) (+ (/ @x 5) (/ dyy 1.7) )))
          (setq p4 (polar al (+ (+ (/ pi 2) pi) ag1) (+ (/ @x 5) (/ dyy 1.7) )))

          (command "pline" p1 p3 p2 p4 "c")
          (setq k (1+ k))
         ) ;repeat
         )
;--------------------------------------삼각형
         ((= kw "5")
         (repeat n
          (setq en (ssname ss k))
          (setq elist (entget en))
          (setq @p1 (car (textbox elist)))
          (setq @p2 (cadr (textbox elist)))
          (setq @x (car @p2) @y (cadr @p2) )
          (setq dy (/ @y 4) dx (/ @y 2));->간격조정
          (setq ag1 (cdr (assoc 50 elist)))
          (setq al (cdr (assoc 11 elist)))

          (setq op1 (polar al (- ag1 (/ pi 2)) (+ dy (/ @y 2))))
          (setq p1 (polar op1 (- ag1 pi) (+ dx (/ @x 2))))

          (setq dis (* (distance p1 al) 1.5))
          (setq p2 (polar al (+ (/ pi 2) ag1) dis))
         
          (command "polygon" 3 al "i" p2)
          (setq k (1+ k))
         ) ;repeat
         )

;--------------------------------------직육각형
         ((= kw "6")
         (repeat n
          (setq en (ssname ss k))
          (setq elist (entget en))
          (setq @p1 (car (textbox elist)))
          (setq @p2 (cadr (textbox elist)))
          (setq @x (car @p2) @y (cadr @p2) )
          (setq dy (/ @y 4) dx (/ @y 2));->간격조정
          (setq ag1 (cdr (assoc 50 elist)))
          (setq al (cdr (assoc 11 elist)))
          
          (setq op1 (polar al (- ag1 (/ pi 2)) (+ dy (/ @y 2))))
          (setq p1 (polar op1 (- ag1 pi) (+ dx (/ @x 2))))
          (setq p2 (polar p1 ag1 (+ @x (* dx 2))))
          (setq p3 (polar p2 (+ ag1 (/ pi 2)) (+ @y (* dy 2))))
          (setq p4 (polar p3 (+ ag1 pi) (+ @x (* dx 2))))

          (setq #1 (polar al (+ pi ag1) (+ (+ (/ @x 2) dx) (+ dx (/ @y 2))) ))
          (setq #2 (polar al ag1 (+ (+ (/ @x 2) dx) (+ dx (/ @y 2))) ))

          (command "pline" p1 p2 #2 p3 p4 #1 "c")
          (setq k (1+ k))
         ) ;repeat
         )
;--------------------------------------정육각형
         ((= kw "7")
         (repeat n
          (setq en (ssname ss k))
          (setq elist (entget en))
          (setq @p1 (car (textbox elist)))
          (setq @p2 (cadr (textbox elist)))
          (setq @x (car @p2) @y (cadr @p2) )
          (setq dy (/ @y 4) dx (/ @y 2));->간격조정
          (setq ag1 (cdr (assoc 50 elist)))
          (setq al (cdr (assoc 11 elist)))
          (setq dxx (+ dx (/ @x 2)))  
        
          (setq p1 (polar al (+ (+ (/ pi 2) (/ pi 6)) ag1) dxx))
          (command "polygon" 6 al "i" p1)
          (setq k (1+ k))
         ) ;repeat
         )
;--------------------------------------사각형
         (t
         (repeat n
          (setq en (ssname ss k))
          (setq elist (entget en))
          (setq @p1 (car (textbox elist)))
          (setq @p2 (cadr (textbox elist)))
          (setq @x (car @p2) @y (cadr @p2) )
          (setq dy (/ @y 4) dx (/ @y 2));->간격조정
          (setq ag1 (cdr (assoc 50 elist)))
          (setq al (cdr (assoc 11 elist)))

          (setq op1 (polar al (- ag1 (/ pi 2)) (+ dy (/ @y 2))))
          (setq p1 (polar op1 (- ag1 pi) (+ dx (/ @x 2))))
          (setq p2 (polar p1 ag1 (+ @x (* dx 2))))
          (setq p3 (polar p2 (+ ag1 (/ pi 2)) (+ @y (* dy 2))))
          (setq p4 (polar p3 (+ ag1 pi) (+ @x (* dx 2))))

          (command "pline" p1 p2 p3 p4 "c")
          (setq k (1+ k))
         ) ;repeat
         )
;--------------------------------------
        );cond
	(command "undo" "end")	;	undo-end
        (setvar "osmode" os)
	(prin1)

) ;defun