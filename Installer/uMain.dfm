object FrmInstall: TFrmInstall
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'RfUtils - Replace Find Unit'
  ClientHeight = 190
  ClientWidth = 565
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lblDescription: TLabel
    Left = 16
    Top = 157
    Width = 63
    Height = 13
    Caption = 'lblDescription'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsItalic]
    ParentFont = False
    Visible = False
  end
  object grpDelphiVersions: TGroupBox
    Left = 0
    Top = 32
    Width = 565
    Height = 105
    Align = alTop
    Caption = 'Delphi Versions'
    TabOrder = 1
    object lvDelphis: TListView
      Left = 2
      Top = 15
      Width = 561
      Height = 88
      Align = alClient
      Columns = <
        item
          Caption = 'Delphi'
          MaxWidth = 150
          MinWidth = 150
          Width = 150
        end
        item
          Caption = 'Path'
          MinWidth = 340
          Width = 340
        end>
      LargeImages = ilDelphiIcons
      ReadOnly = True
      RowSelect = True
      SmallImages = ilDelphiIcons
      TabOrder = 0
      ViewStyle = vsReport
      OnDblClick = lvDelphisDblClick
    end
  end
  object pnlDesc: TPanel
    Left = 0
    Top = 0
    Width = 565
    Height = 32
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object lblVersion: TLabel
      Left = 16
      Top = 11
      Width = 250
      Height = 13
      Caption = 'Select the Delphi'#39's desirable version and click Install:'
    end
  end
  object btnInstall: TButton
    Left = 432
    Top = 152
    Width = 112
    Height = 25
    Caption = 'Install'
    TabOrder = 2
    OnClick = btnInstallClick
  end
  object ilDelphiIcons: TImageList
    Left = 363
    Top = 65
  end
end
