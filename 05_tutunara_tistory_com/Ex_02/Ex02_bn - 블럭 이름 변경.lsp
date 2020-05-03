;; Title : Block Name Change
;; Shoutcut key : bn
;; Description : 블럭을 선택하여 블럭이름을 바꾸는 명령어
;; Maker : 원작(2006.10. 주말농부)
;;         rev 1. 2007.03 / DCL을 이용한 이름 변경(by 주말농부)
;;         rev 2. 2009.05 / DCL을 LSP단일 파일로 흡수 & Code 정리 (by nadau)


(defun c:bn(/ myerror os ss1 en1 blist obn nbn dcl_id fn fname)

  (vl-load-com)
  ;start --- Internal error handler -------------------
   (defun myerror(S)
   (if (/= s "Function cancelled")(princ (strcat "\nError:" s)))
   (setvar "osmode" os)
   (setq *error* olderr)(princ) )
   (setq olderr *error* *error* myerror)
  ;end----------------------------------------
  
   (setvar "cmdecho" 0)
   (create_dialog_bn)
   (setq dcl_id (load_dialog fname))

   (setq os (getvar "osmode"))(terpri)   
   (setq ss1 (entsel "\n이름을 변경할 블럭을 선택하시오. :"))
   (if (= ss1 nil)  ; Null 값에 대한 오류 처리 루틴
      (progn 
          (prompt "\n블럭을 선택하여야 합니다.")
      )
      (progn
       (setq en1 (car ss1)) ;entity name
       (if (eq (cdr (assoc 0 (entget en1))) "INSERT")
           (progn
             (setq blist (assoc 2 (entget en1))) ; block name list
             (setq obn (cdr blist)) ;block name
             (if (not (new_dialog "temp" dcl_id))(exit))  ;; Dialog Display
             (set_tile "text_edit" obn)
             (action_tile "text_edit" "(setq nbn $value)")
             (action_tile "accept" "(done_dialog)")
             (action_tile "cancel" "(done_dialog)")
             (start_dialog)
            (if (/= nbn obn) 
             (progn
                (command "rename" "b" obn nbn)
                (prompt "\n블럭이름이 변경됨 : ")
                (prin1 obn) (prompt " -> ") (prin1 nbn)
             ))
          )
          (prompt "\n블럭을 선택하여야 합니다.")
      );if end 
   ))
   (unload_dialog dcl_id)
   (setvar "cmdecho" 1)
   (prin1)
)

(defun create_dialog_bn()
 (setq fname (vl-filename-mktemp "dcl.dcl"))
 (setq fn (open fname "w"))
 (write-line "temp
   : dialog {
	  label = \"Block Name Change\";
	  initial_focus = \"text_edit\";
	 : edit_box {
		label = \"Name:\";
		key = \"text_edit\";
		edit_width = 40;
		edit_limit = 250;
		allow_accept = true;
	 }
	 ok_cancel;
   }
  " fn)
  (close fn) 
);
(princ)
