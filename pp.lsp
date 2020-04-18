(defun findplotarea (blocks / en lst ipt ipt&en data xsort xysort)
	(while
		(setq en (ssname blocks 0))
		(setq lst (entget en))
		(setq ipt (cdr (assoc 10 lst)))
		(setq ipt&en (cons ipt en))
		(setq data (cons ipt&en data))
		(ssdel en blocks)
	)
	(setq xsort (vl-sort data (function (lambda (a b) (< (caar a) (caar b))))))
	(setq xysort (vl-sort xsort (function (lambda (a b) (> (cadar a) (cadar b))))))
)

(defun c:pp(/ ss plotlist num vlename min max llp urp width height landscape)
	(setvar "cmdecho" 0)
	(setq ss (ssget '((0 . "insert"))))
	(setq plotlist (findplotarea ss))
	;(setq num 0)
	(repeat (length plotlist)
		(setq vlename (vlax-ename->vla-object (cdar plotlist)))
		;(setq vlename (vlax-ename->vla-object (cdr (nth num plotlist))))
		(vla-getboundingbox vlename 'min 'max)
		(setq llp (vlax-safearray->list min))
		(setq urp (vlax-safearray->list max))
		(setq width (- (car urp) (car llp)))
		(setq height (- (cadr urp) (cadr llp)))
		(if
			(> width height)
			(setq landscape "l")
			(setq landscape "p")
		)
		(command	"._plot"
					"Y"								;상세 구성
					""								;모형
					"DocuCentre-IV C2263"						;플로터 이름
					"ISO 전체 페이지 A4(297.00 x 210.00 mm)"			;용지 크기
					"m"								;용지 단위 밀리미터
					landscape							;가로 세로 판단 landscape, portrait
					"N"								;위 아래 반전
					"W"								;인쇄 영역 - 윈도우 입력
					llp								;왼쪽 아래 점 lowleftpoint
					urp								;오른쪽 위 점 uprightpoint
					"f"								;
					"c"								;플롯 간격 띄우기 - 중심(c)
					"Y"								;플롯 스타일 사용 여부
					"monochrome.ctb"						;플롯 스타일
					"y"								;선 가중치 적용 여부
					"a"								;음영 설정 - 표시되는 대로(A)/와이어프레임(W)/숨김(H)/뷰 스타일(V)/렌더(R)
					"n"								;파일로 저장 여부
					"n"								;페이지 설정 저장 여부
					"y"								;플롯 진행 여부
		)
		;(setq num (1+ num))
		(setq plotlist (cdr plotlist))
	)
)

(defun c:ppdf(/ ss plotlist filename pdfnameprefix pdfpath pdfname vlename min max llp urp width height landscape)
	(setvar "cmdecho" 0)
	(setq ss (ssget '((0 . "insert"))))
	(setq plotlist (findplotarea ss))
	(setq filename (getvar "dwgname"))
	(setq pdfnameprefix (substr filename 1 (- (strlen filename) 4)))
	(setq pdfpath (strcat (getvar "dwgprefix") pdfnameprefix))
	;(setq pdfpath (acet-ui-pickdir "새 폴더를 만들순 없어요" (getvar "dwgprefix") "pdf를 저장할 폴더를 선택하세요"));expresstools 설치해야 사용가능
	(repeat (length plotlist)
		(setq pdfname (strcat pdfpath "-" (rtos (getvar "cdate") 2 6) ".pdf"))
		(setq vlename (vlax-ename->vla-object (cdar plotlist)))
		(vla-getboundingbox vlename 'min 'max)
		(setq llp (vlax-safearray->list min))
		(setq urp (vlax-safearray->list max))
		(setq width (- (car urp) (car llp)))
		(setq height (- (cadr urp) (cadr llp)))
		(if
			(> width height)
			(setq landscape "l")
			(setq landscape "p")
		)
		(command	"._plot"
					"Y"											;상세 구성
					""											;모형
					"DWG To PDF.pc3"						;플로터 이름
					"ISO 전체 페이지 A4(297.00 x 210.00 mm)"			;용지 크기
					"m"								;용지 단위 밀리미터
					landscape							;가로 세로 판단 landscape, portrait
					"N"								;위 아래 반전
					"W"								;인쇄 영역 - 윈도우 입력
					llp								;왼쪽 아래 점 lowleftpoint
					urp								;오른쪽 위 점 uprightpoint
					"f"								;
					"c"								;플롯 간격 띄우기 - 중심(c)
					"Y"								;플롯 스타일 사용 여부
					"monochrome.ctb"						;플롯 스타일
					"y"								;선 가중치 적용 여부
					"a"								;음영 설정 - 표시되는 대로(A)/와이어프레임(W)/숨김(H)/뷰 스타일(V)/렌더(R)
					pdfname								;pdf파일 저장
					"n"								;페이지 설정 저장 여부
					"y"								;플롯 진행 여부
		)
		(command "delay" 1000)
		(setq plotlist (cdr plotlist))
	)
)



	   
	   