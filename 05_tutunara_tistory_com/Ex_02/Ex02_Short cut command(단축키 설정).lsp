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

; ���ü� �Է�
(DEFUN C:Q()
  (COMMAND "QLEADER")
  (PRINC)
)
; ���ü� �Է� / �ѱ�
(DEFUN C:��()
  (COMMAND "QLEADER")
  (PRINC)
)


; Ʈ��
(DEFUN C:T()
(COMMAND "TRIM")
(PRINC)
)
; Ʈ��
(DEFUN C:��()
(COMMAND "TRIM")
(PRINC)
)


; ����
(DEFUN C:��()
(COMMAND "EXPLODE")
(PRINC)
)

;
(DEFUN C:TT()
(COMMAND "TEXT")
(PRINC)
)
;
(DEFUN C:����()
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

; ������ ����
(DEFUN C:dsa()
(COMMAND "dimdisassociate")
(PRINC)
)