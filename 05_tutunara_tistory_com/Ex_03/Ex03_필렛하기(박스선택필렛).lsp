;;================================================================
;  박스선택 fillet 하기
;;----------------------------------------------------------------
(defun c:q1(/ r1 SPT SEP wh )
	(setq wh 20)
	(while wh
	(SETQ SPT (GETPOINT "\n>> PICK FIRST POINT :"))
	(SETQ SEP (GETCORNER SPT "\n>> PICK SECOND POINT :"))
	(if (null SPT) (setq wh nil))
	(command "fillet" "r" 0 "")(command "fillet" "c" SPT SEP)
	)
)

(defun c:q2(/ r1 SPT SEP wh)
	(setq wh 20)
	(setq r1(getreal "\n몇 알(radius)?: "))
	(while wh
	(SETQ SPT (GETPOINT "\n>> PICK FIRST POINT :"))
	(SETQ SEP (GETCORNER SPT "\n>> PICK SECOND POINT :"))
	(if (null SPT) (setq wh nil))
	(command "fillet" "r" r1 "")(command "fillet" "c" SPT SEP)
	)
)