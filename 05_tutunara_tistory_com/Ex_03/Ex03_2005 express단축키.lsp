;**************************************************************
;
;                       단축키 생성 명령
;                      
;              ※  express tool 이 설치되어 있어야 합니다.
;*************************************************************

(defun c:va() (c:layon ))    ;  va - 모든 레이어 켜기

(defun c:ff() (c:layoff))       ;   ff - 레이어 끄기
(defun c:vv() (c:layiso))    ;   vv - 레이어 남기기 
(defun c:cv() (c:laycur))    ;  cv - 오브젝트의 레이어를 현재레이어로
(defun c:cf() (c:layfrz))  ;  cf - 레이어 얼리기
(defun c:ll() (c:_laylck))  ;  ll - 레이어 락
(defun c:ul() (c:_layulk))  ;  ul - 레이어 락풀기
(defun c:af() (c:lman))       ;  af - 레이어 매니저 (온 오프 상태 저장)
(defun c:er() (c:extrim))     ;  er - 쿠키커터 trim 
(defun c:mc() (c:copym))     ;  mc - 멀티카피
(defun c:nc() (c:ncopy))     ;  nc - 블럭내 객체 카피
(defun c:ttm() (c:textmask))     ;  text 마스크
(defun c:act() (c:ARCTEXT))     ;  at - 아크 텍스트

(princ)
