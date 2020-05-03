(vl-load-com)

(defun sl-mapcar-ss (func ss)
	(mapcar func (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss))))
)

(defun sl-dub-del (lst / ) 
	(if lst (cons (car lst) (sl-dub-del (vl-remove (car lst) (cdr lst)))))
)

;;;���� ���̾� �ѱ�
(defun c:ly( / ss layerList layers)
	(vl-load-com)
	(if (setq ss (ssget))
		(progn
			(setq layerList (sl-dub-del (sl-mapcar-ss '(lambda (en) (cdr (assoc 8 (entget en)))) ss)))
			(setq layers (vlax-get (vla-get-activedocument (vlax-get-acad-object)) 'layers))
			(vlax-for layer layers
				(if (vl-position (vlax-get layer 'name) layerList)
					(vlax-put layer 'LayerOn -1)
					(vlax-put layer 'LayerOn 0)
				)
			)
			(vlax-release-object layers)
		)
	)
	(princ)
)

;;;��� ���̾� �ѱ�
(defun c:lo( / layers)
	(vl-load-com)
	(setq layers (vlax-get (vla-get-activedocument (vlax-get-acad-object)) 'layers))
	(vlax-for layer layers
		(vlax-put layer 'LayerOn -1)			
	)
	(vlax-release-object layers)
	(princ)
)

;;;���� ���̾� ����
(defun c:lf( / ss layerList layers)
	(vl-load-com)
	(if (setq ss (ssget))
		(progn
			(setq layerList (sl-dub-del (sl-mapcar-ss '(lambda (en) (cdr (assoc 8 (entget en)))) ss)))
			(setq layers (vlax-get (vla-get-activedocument (vlax-get-acad-object)) 'layers))
			(foreach layer layerList
				(vlax-put (vla-item layers layer) 'LayerOn 0)
			)
			(vlax-release-object layers)
		)
	)
	(princ)
)