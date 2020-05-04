;; ─────  리스트 내부의 중복되는 요소삭제
;;  (Sub_OverlapLispDel (list 0 1 3 2 2 3  1 4 5))
;;  (0 2 3 1 4 5)
(defun Sub_OverlapLispDel (InList / ExList)
    (foreach item InList
        (if (not (member item ExList))
            (setq ExList (cons item ExList))
        )
    )
    (reverse ExList)
)
 
;; ─────  2개의 리스트를 비교하는 중복되는 요소삭제
;;  (Sub_MinusList (list 0 1 2 3 4 5)(list 0  2 4))
;;  (1 3 5)
(defun Sub_MinusList (AList BList / ExList)
    (setq ExList nil)
    (foreach item Alist
        (if (not (member item Blist))
            (setq ExList (cons item ExList))
        )
    )
    (reverse ExList)
)
;; ─────  리스트를 구분자로 결합하여 글자 제작
;;  (Sub_List>Sting (list "AA" "11" "BB" "22" "CC" "33""DD" "44") " / ")
;;  "AA / 11 / BB / 22 / CC / 33 / DD / 44"
(defun Sub_List>Sting (LayerList ctr / LayerText)
    (setq LayerText nil)
    (foreach item LayerList
        (if (eq LayerText nil)
            (setq LayerText item)
            (setq LayerText (strcat LayerText ctr item))
        )
    )
    LayerText
)
  
;; ─────  글자를 구분자로 분리하여 리스트 제작
;;  (Sub_Sting>List "AA / 11 / BB / 22 / CC / 33 / DD / 44" " / ")
;;  ("AA" "11" "BB" "22" "CC" "33" "DD" "44")
(defun Sub_Sting>List (str ctr / sn newList)
    (while (setq sn (vl-string-search ctr str))
        (setq newList (cons (substr str 1 sn) newList))
        (setq str (substr str (+ (+ (strlen ctr) sn) 1)))
    )
    (reverse (cons str newList))
)
  
;; ─────  리스트에서를 N번째요소 교체 추가
;;  (Sub_EditList 'Chg 2 (list "A" "B" "C" "D" "A" "B") "X")
;;  ("A" "X" "C" "D" "A" "B")
;;  (Sub_EditList 'Ins 2 (list "A" "B" "C" "D" "A" "B") "X")
;;  ("A" "X" "B" "C" "D" "A" "B")
(defun Sub_EditList (Opt Num Lst Put / Rtn)
    (setq Rtn nil)
    (foreach item Lst
        (setq Num (1- Num))
        (cond
            ((not (zerop Num))(setq Rtn (cons item Rtn)))
            ((and (zerop Num)(eq Opt 'Chg))(setq Rtn (cons Put Rtn)))
            ((and (zerop Num)(eq Opt 'Ins))(setq Rtn (cons item (cons Put Rtn))))
        )
    )
    (reverse Rtn)
)
;; ─────  리스트에서를 N번째요소 삭제
;;  (Sub_RemoveNth 3 (list 0 1 2 3 4 5))
;;  (0 1 3 4 5)
(defun Sub_RemoveNth (Num InList)
    (vl-remove-if '(lambda (x) (zerop (setq Num (1- Num)))) InList)

