
(defun soojeon (ins str)
    (setq ins (getpoint))
    (entmakex
        (list
            '(000 . "TEXT")
            (cons 8  "수정넘버")
            (cons 6  "Continuous")
            (cons 010 ins)
            (cons 040 0.75)
            (cons 001 str)
        )
    )
)






