;
; 선 분할 (점 갯수 나누기)
;
;    만든이 : 이혁진
;

(defun c:DL(/ ct no len c2 cla s1 s2 e1 e2 int msx msy mex mey i e1 e2 s1 s2 sx sy ex ey  po1 )
    ( prompt "\n select 2-LINES : ... " )   ; selection entity
    (setq os (getvar "osmode"))
    (setvar "osmode" 0)
    (command "undo" "be")
    ( setq ct ( ssget '((0 . "LINE"))))
    ( SETQ no  ( GETINT "\n DIVIDING NUMBER : ... ")
           len ( sslength ct ) cl1 ( ssname ct 0 )
           cl2 ( ssname ct 1 ) c1 ( entget cl1 )
           c2  ( entget cl2 )
           cla ( cdr ( assoc 8 c1 ))
           s1  ( cdr ( assoc 10 c1 ))
           e1  ( cdr ( assoc 11 c1 ))
           s2  ( cdr ( assoc 10 c2 ))
           e2  ( cdr ( assoc 11 c2 ))
           int ( inters s1 e2 e1 s2 ) )
    ( if (= int nil ) ( setq k s2 s2 e2 e2 k ))
    ( setq msx (/ (- (car s2) (car s1))  no )
          msy (/ (- (cadr s2) (cadr s1)) no )
          mex (/ (- (car e2) (car e1))   no )
          mey (/ (- (cadr e2) (cadr e1)) no )
          i 1 )

   ( REPEAT (- NO 1)
     ( setq sx (+ (car  s1) (* msx i))
            sy (+ (cadr s1) (* msy i))
            ex (+ (car  e1) (* mex i))
            ey (+ (cadr e1) (* mey i))  i (+ i 1)
            po1 (list sx sy) po2 (list ex ey) )
     ( COMMAND "line" (trans po1 0 1) (trans po2 0 1) "" )
   )
(command "undo" "e")
(setvar "osmode" os)
(princ)
)


