;;=======================================================
;  �ָ��� �ڷ� ������(2007.07.28 �ָ����)
;  ->hatch,trace�� �ۼ��� ��ü�� �ڷ� ������
;    text,mtext,dimension�� ������ ������ ��ɾ�
;;------ draworder --------------------------------------
(defun c:do(/ ss)
   (prompt " �ָ��� �ڷ� ������...")
   (setq ss (ssget "x" '((0 . "HATCH,TRACE"))) )
   (vl-cmdf "draworder" ss "" "b")
   (setq ss (ssget "x" '((0 . "TEXT,MTEXT,DIMENSION"))) )
   (vl-cmdf "draworder" ss "" "f")
   (prompt "\n�ָ����� �ڷ� �ؽ�Ʈ�� ������ ����.")
(prin1))