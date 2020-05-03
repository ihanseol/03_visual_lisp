;[블럭만들어 제자리붙이기]
;BLOCk만들기-블럭이름 묻지도,따지지도 않고 날자와시간을 이름으로...
;날자와시간은 10진법을 적용하여 실수로 표현됨
;예) 고정이름 + 2010년 02월 09일 오후 7시 05분 17초 -> [ 고정이름-20100209.190517 ] 로 작성됨
;by ysJeong 2010.02.19

(defun c:bb (/ os ent bp blk_nme obn ins rnam byn byn2 date fn_date)

  (defun *error* (msg)(princ "error: ")(princ msg)

  (setvar "osmode" os) (princ))

   (graphscr)(terpri) (setvar "CMDECHO" 0)
   (setq os (getvar "osmode"))
   (setq date (getvar "cdate"))
   (setq fn_date (rtos date 2 6))
   (prompt "≫ 블럭으로 만들 객체를 선택을 하세요... ")
   (setq ent (ssget ))
   (setq bp (getpoint "\n≫ 삽입점 클릭하세요 : "))(terpri)
   (command "_copybase" bp ent "" "_pasteblock" bp "erase" ent "")  
      (setq ent (entlast))
        (setq elist (entget ent))
        (setq obn (cdr (assoc 2 elist)))
        (setq ins (cdr (assoc 10 elist)))
            (command "explode" ent)
            (setvar "osmode" 0)
            (setq b_name (strcat "ABC-" fn_date)) ; 여기서 ABC- 를 자신의 네임- 로 바꾸시면 됩니다.
            (command "_.block" b_name ins "P" "")
            (command "_.insert" b_name ins "" "" "")
            (princ "≫ 블록이름 : ") (princ "[ ")(princ b_name)(princ " ]")(princ "(으)로 작성되었습니다.") 
   (setvar "osmode" os)
   (princ)
   );defun 




;; 제자리 블럭잡기 (블럭 이름 설정)
(defun c:bb1 (/ os ent bp blk_nme obn ins rnam byn byn2)

 (defun *error* (msg)
   (princ "error: ")
   (princ msg)
   (setvar "osmode" os)
 (princ))

   (graphscr)(terpri) (setvar "CMDECHO" 0)
   (setq os (getvar "osmode"))
   (prompt "≫ 블럭으로 만들 객체를 선택을 하세요... ")
   (setq ent (ssget ))
   (setq bp (getpoint "\n≫ 삽입점 클릭하세요 : "))(terpri)
   (command "_copybase" bp ent "" "_pasteblock" bp "erase" ent "")  
   (setq blk_nme (getstring "\n≫ 블럭이름을 지정하시겠습니까? [ Yes/No(Esc) ] [Y]  : "))   
   (if (or (= blk_nme "")(= blk_nme "Y") (= blk_nme "y"))
   ;;블럭 이름 설정
      (if (setq ent (entlast))
   (progn (setq elist (entget ent))
        (setq obn (cdr (assoc 2 elist)))
        (setq ins (cdr (assoc 10 elist)))
        (setq rname nil)
        (while (= rname nil)
          (setq rname (getstring t "\n≫ 블럭이름을 입력하세요 : "))
          (setq byn (assoc 2 (tblsearch "block" rname)))
          (setq byn2 (cdr byn)) ; byn2 블럭이름
          (if (= rname byn2) 
            (progn (setq rname nil)
               (prompt "≫ 중복된 이름입니다 -> 다시 입력해 주세요")
            ) ;progn
          );if 
        ) ;while
        (if (/= rname "") 
          (progn (command "explode" ent)
            (setvar "osmode" 0)
            (command "_.block" rname ins "P" "")
            (command "_.insert" rname ins "" "" "")
            (setvar "osmode" os)
          (princ "≫ 블록이름 : ")(princ "[ ")(princ obn)(princ " ]")(princ " 에서 ")
    (princ "[ ")(princ rname)(princ " ]")(princ "(으)로 설정되었습니다.") 
          );progn
        );if 
      );progn
     );if 
   ;;블럭 이름 설정 끝
   );if
   (setvar "osmode" os)
   (princ)
   );defun