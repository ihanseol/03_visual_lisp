;;
;; ** ������ ��ü ����Ÿ�� ������ ���� �ϱ� **
;; 
;; Modify : - Linetype scale �Է� �ް� ����  by nadau 2009-0806 

(defun c:ct(/ #a #b #index)
(setq #a (ssget))

;(setq #b 1) ; �����ϰ� 1
 (setq #b (getreal "\n������ LineType Scale�� �Է��Ͻÿ� :") )

(setq #index 0)
  (if #b (repeat (sslength #a)
       (vla-put-linetypescale(vlax-ename->vla-object(ssname #a #index)) #b )
       (setq #index (1+ #index))
))(princ) );;repeat,if,defun
