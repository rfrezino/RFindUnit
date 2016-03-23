object frmSettings: TfrmSettings
  Left = 0
  Top = 0
  Caption = 'Settings'
  ClientHeight = 344
  ClientWidth = 446
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object pgcMain: TPageControl
    Left = 0
    Top = 0
    Width = 446
    Height = 344
    ActivePage = tsAutoImport
    Align = alClient
    TabOrder = 0
    object tsAutoImport: TTabSheet
      Caption = 'Auto Import'
    end
  end
end
