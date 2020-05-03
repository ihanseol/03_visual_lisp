;;==============================================================
;  도면층 병합하기 (2007.7.12 주말농부)
;  ->병합할 도면층 다중선택후 대상 도면층을 선택하면 객체을 모두
;    대상 도면층으로 변경후 병합할 도면층을 삭제함.
;  ->Undo기능 가능
;
;  modify : dcl 필요없이 단일 실행 가능하게 수정 by nadau(2010.4.12)
;------- layer merge -------------------------------------------

(defun c:lm(/ oln nln kk ky1 la-lst la1 la2 n fn fname)

(vl-load-com)
(create_dialog_lm)

   (prompt "* 도면층 병합하기...")
   
(setq dcl_id (load_dialog fname))

   (setq oln nil nln nil kk 0)
   (setvar "clayer" "0")

   (setq ky1 9)
(while (= ky1 9)
   (if (not (new_dialog "temp" dcl_id)) (exit))
   (setq la-lst (@lm_lst))
   (start_list "k1_lst" 3)(mapcar 'add_list la-lst)(end_list)
   (start_list "k2_lst" 3)(mapcar 'add_list la-lst)(end_list)

   (action_tile "k1_lst" "(setq oln $value)")
   (action_tile "k2_lst" "(setq nln $value)")
   (action_tile "ch_ky1" "(setq ky1 9)(done_dialog)")
   (action_tile "ch_ky2" "(setq ky1 4)(done_dialog)")
   (action_tile "accept" "(setq ky1 1)(done_dialog)")
   (start_dialog)
   
;->merge start
   (if (= ky1 9) (if (and oln nln)(progn
         (setq la2 (nth (atoi nln) la-lst))
         (setq k1lst (@tx_num_lst oln))
         (setq n 0)
         (command "undo" "be")
         (repeat (length k1lst)
            (setq k1 (fix (nth n k1lst)))
            (setq la1 (nth k1 la-lst))
            (command "laymrg" "n" la1 "" "n" la2 "y")
            (setq n (+ n 1)) )
        (command "undo" "e")
        (setq kk (+ kk 1))
   ) ) )
;<-merge end
   (if (and (= ky1 4) (> kk 0))
       (progn (setq ky1 9 kk (- kk 1))(command "u"))
   )
   (setq oln nil nln nil)
);while end

   (unload_dialog dcl_id)
   (vl-file-delete fname)
   (princ)
);


;subroutine
;레이어의 목록을 만드는 리습
(defun @lm_lst(/ la1 la-lst)
   (setq la-lst '())
   (setq la1 (tblnext "layer" t))
   (setq la1 (tblnext "layer"))
   (while la1
       (setq la1 (cdr (cadr la1)));layer name
       (if (/= (wcmatch la1 "*|*") T) (setq la-lst (cons la1 la-lst)) )
       (setq la1 (tblnext "layer"))
   )
   (setq la-lst (vl-sort la-lst '<))
)

;;========================================================
;  숫자 리스트 작성(2007.05.주말농부)
;  ->문자중 숫자만의 리스트을 만드는 리습 (tx->txnum-lst)
;;--------------------------------------------------------
(defun @tx_num_lst(tx / txn k tx1 tx2 tx3 txnum-lst)
  (setq txn (strlen tx)  tx2 "" txnum-lst nil k 1  )
  (repeat (+ txn 1)
    (setq tx1 (substr tx k 1))
    (if (or (= 46 (ascii tx1)) (<= 48 (ascii tx1) 57))
        (setq tx2 (strcat tx2 tx1))
        (progn
          (if (/= tx2 "")(setq txnum-lst (append txnum-lst (list (atof tx2)))) )
          (setq tx2 "")
        )  )
    (setq k (1+ k))
  )
txnum-lst)


;; ------------------------------------------------------ dcl start -----------------------------
(defun create_dialog_lm ()
(setq fname (vl-filename-mktemp "layer_merge.dcl"))
(setq fn (open fname "w"))
(write-line "temp
: dialog {label=\"Layer Merge\";
  :row {
    :boxed_row {
      label=\"병합할 도면층\";
      :list_box {
         key=k1_lst;
         width=22;
         height=11;
         multiple_select=true;
      }
    }
:column {
    fixed_height=true;
    :button {
      label=\"변환하기\";
      key=ch_ky1;
    }
    :button {
      label=\"취소하기\";
      key=ch_ky2;
    }
}
    :boxed_row {
      label=\"대상 도면층\";
      :list_box {
         key=k2_lst;
         width=22;
         height=11;
//         multiple_select=true;
      }
    }
  }
  ok_only;}
" fn)

(close fn)

);defun

;; ------------------------------------------------------ dcl end -----------------------------
