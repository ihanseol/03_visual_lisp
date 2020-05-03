;  
(DEFUN C:CG()
(COMMAND "CHANGE")
(PRINC)
)
;
(DEFUN C:CI()
(COMMAND "CIRCLE")
(PRINC)
)

; 지시선 입력
(DEFUN C:Q()
  (COMMAND "QLEADER")
  (PRINC)
)
; 지시선 입력 / 한글
(DEFUN C:ㅂ()
  (COMMAND "QLEADER")
  (PRINC)
)


; 트림
(DEFUN C:T()
(COMMAND "TRIM")
(PRINC)
)
; 트림
(DEFUN C:ㅅ()
(COMMAND "TRIM")
(PRINC)
)


; 폭파
(DEFUN C:ㅌ()
(COMMAND "EXPLODE")
(PRINC)
)

;
(DEFUN C:TT()
(COMMAND "TEXT")
(PRINC)
)
;
(DEFUN C:ㅅㅅ()
(COMMAND "TEXT")
(PRINC)
)

;
(DEFUN C:dy()
(COMMAND "LENGTHEN" "DY")
(PRINC)
)


;
(DEFUN C:hh()
(COMMAND "xline" "h")
(PRINC)
)

;
(DEFUN C:vv()
(COMMAND "xline" "v")
(PRINC)
)

;
(DEFUN C:qq()
(COMMAND "QDIM")
(PRINC)
)

;
(DEFUN C:fr()
(COMMAND "fillet" "r")
(PRINC)
)

;
(DEFUN C:dd()
(COMMAND "draworder")
(PRINC)
)

; 연관성 해제
(DEFUN C:dsa()
(COMMAND "dimdisassociate")
(PRINC)
)