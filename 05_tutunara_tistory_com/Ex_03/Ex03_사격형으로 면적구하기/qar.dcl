//사각형으로 면적구하기(2007.7.4 주말농부)
qar_dcl :dialog  {
  label="사각형으로 면적구하기";
    :boxed_column {
      label="Quick area result";
      :row {
        :button {
          label="산  식";
          key="butt1";
        }
        :edit_box {
          key="tx-exp";
	  edit_width = 45;
          allow_accept=true;
        }
      }
      :row {
        :button {
          label="결  과";
          key="butt2";
        }
        :edit_box {
          key="tx-rlt";
	  edit_width = 45;
          allow_accept=true;
        }
      }
    }
    :boxed_row {
      label="Select button";
      :radio_button {
        label="대치할 문자선택";
        key="rad1";
      }
      :radio_button {
        label="문자삽입점 지정";
        key="rad2";
      }
    }
  ok_cancel;
}
