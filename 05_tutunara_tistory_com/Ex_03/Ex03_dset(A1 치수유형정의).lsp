;;================================================================
;  Dimension setting (000316 주말농부)
;  -> 건축에서 표준화된 시스템 변수의 정의 및 치수유형 설정
;     A1 도면에 맞는 치수 설정
;;----------------------------------------------------------------
(defun c:dset(/ z zz sc stname)
   (setq sc (getint "\nScale factor=>"))
   (setq z (tblsearch "style" "ezdim"))
   (if z (prompt "\t문자유형=Ezdim 존재")(progn
        (command "style" "ezdim" "romans,whgtxt" "0" "1" "0" "n" "n" "n")
        (prompt "\t문자유형=Ezdim 생성")
   ) )
;선과 화살표<Lines and Arrows>
(SETVAR "DIMCLRD" 256); 치수선과 화살표, 인출선의 색상을 지정한다.<256=bylayer>
(SETVAR "DIMLWD" -1); 치수선에 선가중치를 지정합니다. 
(SETVAR "DIMDLE" 1.50); 화살촉 대신 Tick를 사용할 경우, 치수 보조선 밖으로 연장되는 치수선의 길이를 조정한다.
(SETVAR "DIMDLI" 8.00); BASeline치수 기입시 치수선간의 간격을 조정한다.
(SETVAR "DIMSD1" 0); 첫 번째 치수선을 화면상에 나타나지 않게 한다.
(SETVAR "DIMSD2" 0); 두 번째 치수선을 화면상에 나타나지 않게 한다.
(SETVAR "DIMCLRE" 256); 치수 보조선의 색상을 지정한다.<256=bylayer>
(SETVAR "DIMLWE" -1); 치수보조선에 선가중치를 지정합니다.
(SETVAR "DIMEXE" 1.50); 치수선에서 연장되는 치수 보조선의 길이를 조정한다.
(SETVAR "DIMEXO" 2.00); 외형선과 치수 보조선과의 간격을 조정한다.
(SETVAR "DIMSE1" 0); 첫 번째 치수 보조선을 화면상에 나타나지 않게 한다.
(SETVAR "DIMSE2" 0); 두 번째 치수 보조선을 화면상에 나타나지 않게 한다.
(SETVAR "DIMBLK" "DotSmall")
;(SETVAR "DIMBLK1" "DotSmall"); 첫 번째 화살표의 블록 이름을 지정한다.
;(SETVAR "DIMBLK2" "DotSmall"); 두 번째 화살표의 블록 이름을 지정한다.
(SETVAR "DIMLDRBLK" "DotSmall"); 지시선의 화살표 형태를 지정합니다.
(SETVAR "DIMASZ" 6.00); 치수선 끝의 화살촉 크기를 조정한다.
(SETVAR "DIMCEN" 1.50); 원이나 호의 중심 표시을 설정한다.

;문자<Text>
(SETVAR "DIMTXSTY" "EZDIM"); 치수 기입 문자의 STYLE을 지정한다.
(SETVAR "DIMCLRT" 7); 치수 문자의 색상을 지정한다.
(SETVAR "DIMTXT" 3.00); 치수 문자의 높이를 조정한다.
(SETVAR "DIMTFAC" 1.0000); 치수 문자에 대한 공차 문자의 높이를 조정한다.
;(SETVAR "DIMGAP" 1.00); 문자 주위에 프레임 그리기
(SETVAR "DIMTAD" 1); 치수 문자를 치수선 위에 위치시킨다.
(SETVAR "DIMJUST" 0); 치수선상에서 치수 문자의 자리를 맞춘다.
(SETVAR "DIMGAP" 1.00); DIMTAD가 ON일 때 치수선과 문자 사이의 간격을 조정한다.
(SETVAR "DIMTOH" 0); 치수선 외부의 치수 문자를 치수선과 나란하게 둔다.
(SETVAR "DIMTIH" 0); 치수 문자를 치수선과 나란하게 둔다.

;맞춤<Fit>
(SETVAR "DIMATFIT" 3); 치수보조선 내에 치수 문자와 화살표를 넣을 공간이 충분치 않을 경우 치수 문자와 화살표의 배치 방법을 결정합니다.
(SETVAR "DIMTIX" 1); 치수 문자를 치수 보조선 안에 위치시킨다.
(SETVAR "DIMSOXD" 0); DIMTIX가 ON일 때 치수선이 외부로 돌출되는 것을 억제한다.
(SETVAR "DIMTMOVE" 2); 치수 문자 이동 규칙을 설정합니다.
(SETVAR "DIMSCALE" sc); 치수 요소들의 전체 화면에 대한 축척을 조정한다.
(SETVAR "DIMUPT" 0); 사용자 위치 지정 문자에 대한 옵션을 조정합니다. 
(SETVAR "DIMTOFL" 1); 치수 보조선 내부에 치수선을 둔다.

;1차단위<Primary Units>
(SETVAR "DIMLUNIT" 6); 각도를 제외한 모든 치수 형식에 단위를 설정합니다.
(SETVAR "DIMDEC" 0); 치수의 소수 이하 자릿수를 조정한다.
(SETVAR "DIMFRAC" 0); DIMLUNIT가 4<건축> 또는 5<분수>로 설정된 경우 분수 형식을 설정합니다.
(SETVAR "DIMDSEP" "."); 단위 형식이 십진일 경우 치수를 작성할 때 사용할 단일 문자 소수부 구분자를 지정합니다.
(SETVAR "DIMRND" 0.00); 모든 치수 기입 거리를 지정된 값으로 반올림합니다.
(SETVAR "DIMPOST" ""); 치수 측정 단위에 문자 머리말이나 꼬리말 또는 둘 다를 지정합니다.
(SETVAR "DIMLFAC" 1.0000); 현재 치수값에 대한 축척을 조정한다.
(SETVAR "DIMZIN" 0); 공차의 소수이하 필요없는 0을 억제한다.
(SETVAR "DIMAUNIT" 0); 각도 치수의 단위 형식을 설정합니다. 
(SETVAR "DIMADEC" 2); 각도 치수에 표시되는 정밀도 자릿수를 조정합니다.
(SETVAR "DIMAZIN" 0); 각도 치수에서 0을 억제합니다. 

;대체단위<Alternate Units>
(SETVAR "DIMALT" 0); 치수에서의 대체 단위 표시를 조정합니다.
(SETVAR "DIMALTU" 2); 각도를 제외한 모든 치수 유형의 대체 단위 형식을 설정합니다.
(SETVAR "DIMALTD" 1); 대체 단위에서의 소수부 자릿수를 조정합니다.
(SETVAR "DIMALTF" 25.4); 대체 단위에 대한 승수를 조정합니다. 
(SETVAR "DIMALTRND" 0.00); 
;(SETVAR "DIMAPOST" ""); 각도를 제외한 모든 치수 형식의 대체 치수 측정 단위에 문자 머리말이나 꼬리말 또는 둘 다를 지정합니다. 
(SETVAR "DIMALTZ" 0); 대체 단위 치수 값의 0 억제를 조정합니다.
(SETVAR "DIMAPOST" ""); 각도를 제외한 모든 치수 형식의 대체 치수 측정 단위에 문자 머리말이나 꼬리말 또는 둘 다를 지정합니다. 

;공차<Tolerances>
(SETVAR "DIMTOL" 0); 공차 기입을 ON/OFF한다.
(SETVAR "DIMTDEC" 0); 공차 기입시 공차의 소수 이하 자릿수를 조정한다.
(SETVAR "DIMTP" 0.00); 양수 공차를 기입한다.
(SETVAR "DIMTM" 0.00); DIMTOL이나 DIMLIM이 켜있을 경우 치수 문자에 대해 최대<또는 더 높은> 공차 한계를 설정합니다.
(SETVAR "DIMTFAC" 1.0000); 치수 문자에 대한 공차 문자의 높이를 저정한다.
(SETVAR "DIMTOLJ" 1);
(SETVAR "DIMTZIN" 0); 공차의 소수이하 필요없는 0을 억제한다.
(SETVAR "DIMALTTD" 1); 대체 치수 단위로, 공차값의 소수부 자릿수를 설정합니다. 
(SETVAR "DIMALTTZ" 0); 대체 치수 단위로, 공차값의 소수부 자릿수를 설정합니다.

   (setq stname (strcat "ez-" (itoa sc)))
   (setq zz (tblsearch "dimstyle" stname))
   (if zz (command "dimstyle" "s" stname "y")
          (command "dimstyle" "s" stname)
   )
   (prompt "\t현재치수유형=")(prin1 stname)
   (graphscr)
   (prin1)
)




