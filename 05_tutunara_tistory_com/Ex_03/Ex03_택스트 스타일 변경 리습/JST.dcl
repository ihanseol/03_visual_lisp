jin_text_ts :dialog  {
	label="Change Text Style";
//	:boxed_row {
//		:text {label="���ÿɼ�";}
//		:radio_button {
//			 label="�����Ѱ͸�";
//			 key="jts_ch1";
//			 value=1;
//		}
//		:radio_button {
//			 label="������ü��";
//			 key="jts_ch2";
//			 value=0;
//		}
//	}
	:boxed_row {
		label="ù��° ���� ��������";
		:column {
			:text {label="�����̸�";	}
			:text {label="Name";	 key="jts_lic";	}
		}
		:column {
			:text {label="����۲�";	}
			:text {label="Font,Font";	 key="jts_fontc";	 }
		}
	}
//	:spacer { width=1; }
	:boxed_column {
		label="�۲ú��� �޴� - 1";
		:text {label="��������                Font Name";}
		:row {
			:popup_list{
 //					label="��������";
				key="jts_li";
				width=15;
			}
			:text {
				label="";
				key="jts_font";
				width=30;
			}
		}
		:spacer { width=1; }
	}
//	:boxed_column {
//		label="�۲ú��� �޴� - 2 (������ü)";
//		:text {label="��������                Font Name";}
//		:row {
//			:popup_list{
////				label="��������";
//				key="jts_lia";
//				width=15;
//			}
//			:text {
//				label="";
//				key="jts_fonta";
//				width=30;
//			}
//		}
//		:spacer { width=1; }
//	}
	ok_cancel_help;
}








jin_text_ts2 :dialog  {
	label="Change Text Style by Style";
	:boxed_column {
		label="���鳻 ������ �������� ����";
		:text {label="��������                Font Name";}
		:row {
			:popup_list{
				key="jts2_ts1";
				width=15;
			}
			:edit_box {
				key="jts2_tf1";
				width=25;
			}
		}
		:text {label="";}
	}
	:text {	label="���� STYLE�� �Ʒ��� STYLE�� ��ü��";}
	:boxed_column {
		label="������ ������ �������� ����";
		:text {label="��������                Font Name";}
		:row {
			:popup_list{
				key="jts2_ts2";
				width=15;
			}
			:edit_box {
				key="jts2_tf2";
				width=25;
			}
		}
		:text {label="";}
	}
	ok_cancel;
}
