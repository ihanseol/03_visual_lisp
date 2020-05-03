;;=====================================================================
;  도면정리하기
;  ->Audit, Draworder, Purge,  Point<node> Delete,
;    Layer Filters Delete. Ghost delete.
;  -> ghost 수정보완/공백열만 있는건 모두삭제(2007.12.12)
;  -> 단독 실행형으로 재수정 (2009.05.11) by nadau
;  -> dcl 단축키 적용 (2009.08.03) by nadau
;------- file filtering -----------------------------------------------

(defun c:ad(/ dcl_id tg1 tg2 tg3 tg4 tg5 tg6 tg7 ky fn fname)

(vl-load-com)
(create_dialog_ad)

(prompt "\n** 도면 정리 하기 **\n\n")

(setq dcl_id (load_dialog fname))

(setq ky 4 tg1 "0" tg2 "0" tg3 "0" tg4 "1" tg5 "1" tg6 "1" tg7 "0")

(if (not (new_dialog "temp" dcl_id)) (exit) );if

   (set_tile "tog1" tg1)
   (set_tile "tog2" tg2)
   (set_tile "tog3" tg3)
   (set_tile "tog4" tg4)
   (set_tile "tog5" tg5)
   (set_tile "tog6" tg6)
   (set_tile "tog7" tg7)

   (action_tile "tog1" "(setq tg1 $value)")
   (action_tile "tog2" "(setq tg2 $value)")
   (action_tile "tog3" "(setq tg3 $value)")
   (action_tile "tog4" "(setq tg4 $value)")
   (action_tile "tog5" "(setq tg5 $value)")
   (action_tile "tog6" "(setq tg6 $value)")
   (action_tile "tog7" "(setq tg7 $value)(@ff_select tg7)")
   (action_tile "accept" "(setq ky 9)(done_dialog)")
   (action_tile "cancel" "(done_dialog)")

(start_dialog)
(unload_dialog dcl_id)

  (if (= ky 9)(progn
     (command "undo" "be")
     (if (= tg1 "1") (command "audit" "y"))
     (if (= tg2 "1") (@do))
     (if (= tg3 "1") (command "purge" "a" "*" "n"))
     (if (= tg4 "1") (@pointdelete))
     (if (= tg5 "1") (@layerfilterdelete))
     (if (= tg6 "1") (@ghost))
     (command "undo" "e")
     (prompt "\n\n** 도면정리완료 **"))
     (prompt "\n\n** 도면정리취소 **"))

(vl-file-delete fname)
(princ))


;;subroutine (서브루틴)
(defun @ff_select(tg7)
   (cond
      ((= tg7 "0")(setq tg1 "0" tg2 "0" tg3 "0" tg4 "0" tg5 "0" tg6 "0"))
      ((= tg7 "1")(setq tg1 "1" tg2 "1" tg3 "1" tg4 "1" tg5 "1" tg6 "1"))
   )
   (set_tile "tog1" tg1)(set_tile "tog2" tg2)
   (set_tile "tog3" tg3)(set_tile "tog4" tg4)
   (set_tile "tog5" tg5)(set_tile "tog6" tg6)(set_tile "tog7" tg7)
)


;;솔리드 뒤로 보내기(2007.07.28 주말농부) 객체 없을 때 오류 수정 by nadau(09,08.03)
(defun @do(/ ss ss1)
;   (prompt "\n ** 솔리드 뒤로 보내기 **\n\n")
   (setq ss (ssget "x" '((0 . "HATCH,TRACE,SOLID"))) )
   (if (/= ss nil) (vl-cmdf "draworder" ss "" "b") (prompt "\n-> Hatch, Trace, Solid 객체 없음."))
   
   (setq ss1 (ssget "x" '((0 . "TEXT,MTEXT,DIMENSION"))) )
   (if (/= ss1 nil) (vl-cmdf "draworder" ss "" "f") (prompt "\n-> Text,Dimension 객체 없음."))
   
   (if (or(/= ss nil)(/= ss1 nl)) (prompt "\n솔리드을 뒤로 텍스트을 앞으로 보냄.\n"))
(princ)
) ;defun end


;;레이어 필터 삭제하기
(defun @layerfilterdelete(/ objXDict)
   (setq strKeepWC "")
   (vl-load-com)
   (vl-catch-all-apply
      (function
         (lambda ()
            (setq objXDict (vla-GetExtensionDictionary
                  (vla-get-Layers (vla-get-ActiveDocument (vlax-get-acad-object)))))))
   )
   (cond (objXDict
        (or
         (rrbI:DeleteAllXRecs objXDict "ACAD_LAYERFILTERS" strKeepWC)
         (rrbI:DeleteAllXRecs objXDict "AcLyDictionary" strKeepWC))))
(princ))
(defun rrbI:DeleteAllXRecs  (objXDict dictName strKeepWC / objDict i)
   (vl-catch-all-apply
   (function
      (lambda ()
         (setq objDict (vla-Item objXDict dictName))
         (vlax-for objXRec  objDict
            (cond ((not (and strKeepWC (wcmatch (vla-Get-Name objXRec) strKeepWC)))
               (setq i (1+ (cond (i)
                              (0))))
               (vla-Delete objXRec)))))))
;   (cond (i (princ (strcat "\n" (itoa i) " filters deleted.")))))
    (cond (i (princ (strcat "\n" (itoa i) " 필터가 삭제되었습니다.")))))
 
  
;; pointdelete - 점객체 삭제하기
(defun @pointdelete(/ ss)
   (setq ss (ssget "x" (list (cons 0 "point"))))
   (if ss (progn (command "erase" ss "") (princ "\n-> Point<node> ") (princ (sslength ss)) (princ "개를 삭제하였습니다.\n") )) (princ)
) ;defun end


 ;-->ghost start 유령 객체 삭제
(defun @ghost(/ k j ss ss1 en ed etn x10 x11 lis n dissum dis
                tk tnum tss ten ted ttxt tvar @1 ttk)
   (setvar "cmdecho" 0)
   (setq k 0 j 0 tk 0 tnum 0)

; [[[ SubRoutine Start]]] - 내용이 없는 문자 삭제하기.
   (setq tss (ssadd))
   (setq ss (ssget "x" (list (cons 0 "text,mtext"))))
   (if ss
      (repeat (sslength ss)
         (setq ten (ssname ss tk))
         (setq ted (entget ten))
         (setq ttxt (cdr (assoc 1 ted)))
         (setq ascii_list (vl-string->list ttxt))
         (setq ttk 0 tvar 0)
         (repeat (length ascii_list)
            (setq @1 (nth ttk ascii_list))
            (if (/= @1 32) (setq tvar 1))
            (setq ttk (1+ ttk))
         )
         (if (= tvar 0) (progn (ssadd ten tss) (setq tnum (1+ tnum))))
         (setq tk (1+ tk))
      )
   )
   (if (> tnum 0) (progn (command "erase" tss "") (princ "\n-> 내용이 없는 문자 ") (princ tnum) (princ "개를 삭제하였습니다.")))
; [[[ SubRoutine End ]]]


; [[[ SubRoutine Start]]] - 선길이 "0"인 객체 삭제하기.
   (setq ss1 (ssget "x" (list (cons 0 "line,lwpolyline"))))
   (if ss1 (progn
      (repeat (sslength ss1)
         (setq en (ssname ss1 k)
                  ed (entget en)
                  etn (cdr (assoc 0 ed)))
         (cond ((= etn "LINE")
            (setq x10 (cdr (assoc 10 ed))
                     x11 (cdr (assoc 11 ed)))
            (if (= (distance x10 x11) 0) (progn (command "erase" en "") (setq j (1+ j)))))
            ((= etn "LWPOLYLINE")
               (setq lis (GetPolyVtx ed))
               (setq n 0 dissum 0)
               (repeat (1- (length lis))
                  (setq dis (distance (nth n lis) (nth (1+ n) lis)))
                  (setq dissum (+ dissum dis))
                  (setq n (1+ n))
               )
               (if (= dissum 0) (progn (command "erase" en "") (setq j (1+ j))))
            )
         );cond
      (setq k (+ k 1))
      );repeat
   ));if progn
   (if (> j 0) (progn (princ "\n-> 선길이 0 인 객체 ") (princ j) (princ "개를 삭제하였습니다."))) 

(prompt "\n내용이 없는 유령객체(Text/MText/Line/PLine)를 삭제합니다.\n")
(princ) ); end defun


(defun GetPolyVtx(EntList)
   (setq VtxList '())
   (foreach x EntList
      (if (= (car x) 10) (setq VtxList (append VtxList (list (cdr x)))) )
   )
VtxList)
; [[[ SubRoutine End ]]]


;-->ghost end



;; ------------------------------------------------------ dcl start -----------------------------
(defun create_dialog_ad ()
(setq fname (vl-filename-mktemp "purge_temp.dcl"))
(setq fn (open fname "w"))
(write-line "temp
: dialog { label=\"도면 정리 하기\";
   : boxed_column {
   label = \"실행할 명령 선택\";
        : column {
            : toggle {
                   label=\" 도면감사<&Audit> 하기\";
                   key=\"tog1\";
           }
            : toggle {
                   label=\" 솔리드 뒤로<&Draworder> 보내기\";
                   key=\"tog2\";
            }
            : toggle {
                   label=\" 퍼지<&Purge> 하기\";
                   key=\"tog3\";
            }
            : toggle {
                   label=\" 포인트<&Node> 삭제\";
                   key=\"tog4\";
            }
            : toggle {
                   label=\" 레이어 필터<&Filter> 삭제\";
                   key=\"tog5\";
            }
            : toggle {
                   label=\" 의미 없는 유령객체(&Ghost Object) 삭제\";
                   key=\"tog6\";
            }
           : toggle {
                   label=\" 모두 선택하기(&Select) / 모두 취소하기\";
                   key=\"tog7\";
            }
      }
   }
   ok_cancel;}
" fn)
(close fn)

);defun
(princ)

;; ------------------------------------------------------ dcl end -----------------------------
