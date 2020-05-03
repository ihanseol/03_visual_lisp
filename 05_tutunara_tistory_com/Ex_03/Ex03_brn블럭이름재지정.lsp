; 아키모아 운영진 "행복한하루"
; http://cafe.daum.net/archimore
; 블록이름 개별 변경
; 2007.05.31 / 1차수정 05.31

(defun rtd (a)(/ (* a 180.0) pi))

(defun c:brn(/ os ent elist ins ang)
    (prompt "\n블럭 개별 이름변경..") 
    (setvar "cmdecho" 0)
    (setq la (getvar "clayer"))
    (setq os (getvar "osmode"))
    (if (setq ent (car (entsel "\nBlock select:")))
      (progn (setq elist (entget ent))
        (redraw ent 3)
        (setq obn (cdr (assoc 2 elist)))
        (setq ins (cdr (assoc 10 elist)))
        (setq bco (cdr (assoc 8 elist)))
        (setq x41 (cdr (assoc 41 elist)))
        (setq x42 (cdr (assoc 42 elist)))
        (setq x43 (cdr (assoc 43 elist)))
        (setq ang (rtd (cdr (assoc 50 elist)))) 

        (setq rname nil)

        (while (= rname nil)
          (setq rname (getstring "\nNew block name:"))
          (setq byn (assoc 2 (tblsearch "block" rname)))
          (setq byn2 (cdr byn))
          (if (= rname byn2) 
            (progn (setq rname nil)
               (prompt "블럭이름중복->다시입력해주세요")
            ) ;progn
          );if 
        ) ;while
        (redraw ent 4)
        (if (/= rname "") 
          (progn 
            (command "erase" ent "")
            (setvar "osmode" 0)
            (command "_.insert" obn ins "" "" "")
            (command "explode" "l")
            (setvar "clayer" bco)
            (command "_.block" rname ins "p" "")
            (command "_.insert" rname ins "x" x41 x42 x43 ang)  
            (setvar "clayer" la)
            (setvar "osmode" os)
          (princ "블록이름: ")(princ obn)(princ " --> ")(princ rname)(princ " 로 개별변경함") 
          );progn
        );if 
      );progn
     );if 
(princ)
);defun
