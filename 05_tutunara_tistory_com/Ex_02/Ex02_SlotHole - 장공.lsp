;;������ �߽ɿ��� ��� �׸��� 
(defun c:ss(/ os cla ort cp dia rad len hlen p1 p2 p3 p4 p5 p6) ; ���������� defun ����,��ɾ� JG 

;�Ʒ� ������ ��� ������ ��Ҹ� �ϰų� ���� �߻��� ���� �޼����� ���� osmode, clayer, orthomode �� �ʱⰪ���� �ǵ��� 
(defun *error* (msg)(princ "error: ")(princ msg) 
(setvar "osmode" os) (setvar "clayer" cla)(setvar "orthomode" ort) (princ) ) 
;;���� �ڵ鷯 �� 

(prompt "\n ������ �߽ɿ��� ��� �׸���") ; ȭ�鿡 ������ ���� 

(setq os (getvar "osmode")) ; ������ osmode�� os �Լ��� ����Ŵ 
(setq cla (getvar "clayer")) ;������ layer�� cla �Լ��� ����Ŵ 
(setq ort (getvar "orthomode")) ;������ orthomode�� ort �Լ��� ����Ŵ 
(setvar "osmode" 36) ; Int, cen (�� ���� �Ǵ� �߽��� ����) 

(setq cp (getpoint "\n�� Slot�� �߽����� �������� : ")) ; ����� �Է��� �䱸�ϴ� ���� 

(setvar "osmode" 48) ;qua,int (���� ����� �Ǵ� �߽��� ������ ���ؼ�)

(if (= dia nil) (setq dia 9)) ; dia���� nil�� �� (��)�ʱⰪ 9�� ���� 

(setq sdia dia) ; dia �ʱⰪ 9�� sdia �Լ��� ������ 

(setq dia (getdist (strcat "\n�� Slot�� ���� ��ų�(2P) ���� �Է��ϼ���["(itoa sdia)"] : "))) 
; dia �Լ��� �� �Է��� �䱸 getdist�� �Ÿ���(�Է� �Ǵ� ���콺 ��), strcat,(itoa sdia)���� �ʱ� �����ߴ� ���� ȭ�鿡 �������� �� 

(if (= dia nil) (setq dia sdia)) ; dia�Լ����� �Է��� ���� �� ������ ������ sdia�� ���� ���� dia �Լ��� ��ü�� 

(setq rad (/ dia 2)) ; �Էµ� dia���� ������ ����(�������� �������� �߽����� ���� �ϱ� �ϱ� ���� �غ��۾�) 

(setvar "orthomode" 1) ; �Ÿ��� ���콺�� ���ý� ���� ����� �ϱ����� 

(if (= len nil) (setq len 20)) ;len ���� nil�� �� ������ �ʱⰪ 20�� ���� 

(setq nlen len) ; len �ʱⰪ 20�� nlen �Լ��� ������ 

(setq len (getdist (strcat "\n�� Slot�� ���̸� ��ų�(2P) ���� �Է��ϼ���["(itoa nlen)"] : "))) 

; len �Լ��� �� �Է��� �䱸 getdist�� �Ÿ���(�Է� �Ǵ� ���콺 ��), strcat,(itoa nlen)���� �ʱ� �����ߴ� ���� ȭ�鿡 �������� �� 
(if (= len nil) (setq len nlen)) ; len�Լ����� �Է��� ���� �� ������ ������ nlen�� ���� ���� len �Լ��� ��ü�� 
(setq hlen (/ len 2)) ; �Էµ� len���� ������ ����(�������� ���Ա����� �߽����� ���� �ϱ� �ϱ� ���� �غ��۾�) 

; �Ʒ��� �׸��� �׸��� ���� ������ ��ġ(��)�� ���� ����. 
; ��ġ���Լ�(polar ��, ����-X���� �������� �ϴ� ������� ����), ����) �� ������ 
(setq pt1 (polar cp (/ pi 2) rad)) 

; �� 'pt1'��... Base point 'cp'���� ����90��(3.14/2=1.57radian)������ �������� ������ 'rad' �Ÿ��� ��ġ 
(setq pt2 (polar pt1 0 hlen)) 
; �� 'pt2'��... 'pt1'���� 0���� ������ ���������� ������ 'hlen' �Ÿ��� ��ġ 
(setq pt3 (polar pt2 (+ (/ pi 2) pi) dia)) 
; �� 'pt3'��... 'pt2'���� 270��((3.14/2)+3.14)�� ������ ��������ŭ�� 'dia' �Ÿ��� ��ġ 
(setq pt4 (polar pt3 pi (* hlen 2))) 
; �� 'pt4'��... 'pt3'���� 180��((3.14radian)�� ������ ���������� ������ 'hlen'�� �ι�(len��) �Ÿ��� ��ġ 
(setq pt5 (polar pt4 (/ pi 2) dia)) 
; �� 'pt5'��... 'pt4'���� 90��(3.14/2=1.57radian)�� ������ ��������ŭ�� 'dia' �Ÿ��� ��ġ 
(setvar "osmode" 0) 
; ���� ȭ�鿡�� �۰� �׸� �� none�� �ƴ� �ٸ� osmode�� ���� �ֺ� ��ü���� Ŭ���� �Ǿ� �߻��Ǵ� ��������� �����ϱ� ���� none���� ���� 

;(command "layer" "s" "0" "") ; ����� ���̾� ���� 
(command "pline" pt5 pt2 "a" pt3 "l" pt4 "a" pt5 "") ; Pline ������� ������ �� ���� ����...a�� arc, l�� line 
(command "rotate" (entlast) "" cp "\\") ; �׷��� ������ ��ü(����)�� ȸ��������� ȸ�� ����� �Է��� ���� 
(command "undo" "e") ; �۾��� ���� 

(setvar "osmode" os) ; �ʱ⿡ ����ߴ� osmode ����ġ�� �ǵ��� 
(setvar "clayer" cla) ; �ʱ⿡ ����ߴ� layer ����ġ�� �ǵ��� 
(princ) ;; ������ ���䰪��..nil�� �Ⱥ����ְ� �� 

) ;defun  ��
