; 아키모아 운영진 "행복한하루"
; http://cafe.daum.net/archimore
; 넘버링하기..변경하지말아야할 자릿수 선택가능..
; 주말농부님의 서브루틴 사용
; 2007.09.06

(defun c:num(/ ss num numex str1 k ed txt @1 ntxt str a)
  (prompt "\n 넘버링하기..변경하지말아야할 자릿수 선택가능..")
  (setq ss (ssget (list (cons 0 "text,mtext"))))
  (setq a nil)
  (setq a (getint "\n 선택순서대로 하기<엔터> / 자동정렬 <1>:")) 
  (if (= a 1) (setq ss (@ss_new_lst ss)))
  (setq num (getstring "\n>> Start number :"))
  (setq numex (getint "\n>> 변경되지 말아야하는 자릿수 :"))
  (setq str1 (strlen num))
  (setq k 0)
  (repeat (sslength ss)
   (setq ed (entget (ssname ss k))
         txt (cdr (assoc 1 ed)))
   (setq @1 (substr txt 1 numex))
   (setq ntxt (strcat @1 num))
   (entmod (subst (cons 1 ntxt) (assoc 1 ed) ed))
   (setq k (1+ k))
   (setq num (rtos (1+ (atoi num)) 2 0)) 
   (setq str (strlen num))
   (repeat (- str1 str)
     (setq num (strcat "0" num))
   ) 
  );repeat
(princ)
);defun
    

;;================================================================
;  새로운 선택세트 만들기(2007.04.주말농부)
;  ->글쓰기 순서로 선택세트을 재정렬하는 리습
;;----------------------------------------------------------------
(defun @ss_new_lst (ss / ssnew ssn n en dx dy ss-x1 ss-y1 ss-x2 ss-y2 ss-x3 ss-y3)
   (setq ssn (sslength ss))
   (setq n 0)
   (setq ss-x1 '()  ss-y1 '())
   (repeat ssn
       (setq en (ssname ss n))
       (setq en1x (list en (car (cdr (assoc 10 (entget en)) ) ) ))
       (setq en1y (list en (cadr (cdr (assoc 10 (entget en)) ) ) ))
       (setq ss-x1 (cons en1x ss-x1))
       (setq ss-y1 (cons en1y ss-y1))
       (setq n (+ n 1))
   )
   (setq ss-x2 (vl-sort ss-x1 '(lambda (e1 e2) (< (cadr e1) (cadr e2))))  )
   (setq ss-y2 (vl-sort ss-y1 '(lambda (e1 e2) (> (cadr e1) (cadr e2))))  )
;;;선택방향설정기준
   (setq dx (- (cadr (nth (- ssn 1) ss-x2))  (cadr (nth 0 ss-x2)))   )
   (setq dy (- (cadr (nth 0 ss-y2))   (cadr (nth (- ssn 1) ss-y2)))  )
;;;새로운 선택세트 만들기
   (setq n 0 ss-x3 (ssadd) ss-y3 (ssadd))
   (repeat ssn
      (setq ss-x3 (ssadd (car (nth n ss-x2)) ss-x3))
      (setq ss-y3 (ssadd (car (nth n ss-y2)) ss-y3))
      (setq n (+ n 1))
   )
;;;선택방향설정
   (if (>= (- dx dy) 0) (setq ssnew ss-x3) (setq ssnew ss-y3)  )
)