//�簢������ �������ϱ�(2007.7.4 �ָ����)
qar_dcl :dialog  {
  label="�簢������ �������ϱ�";
    :boxed_column {
      label="Quick area result";
      :row {
        :button {
          label="��  ��";
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
          label="��  ��";
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
        label="��ġ�� ���ڼ���";
        key="rad1";
      }
      :radio_button {
        label="���ڻ����� ����";
        key="rad2";
      }
    }
  ok_cancel;
}
