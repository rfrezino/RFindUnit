object frmFindUnit: TfrmFindUnit
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'Find Unit'
  ClientHeight = 370
  ClientWidth = 359
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  Visible = True
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object grpOptions: TGroupBox
    Left = 0
    Top = 301
    Width = 359
    Height = 69
    Align = alBottom
    Caption = 'Search Options'
    TabOrder = 2
    object chkSearchLibraryPath: TCheckBox
      Left = 19
      Top = 45
      Width = 163
      Height = 17
      Caption = 'Search in Library Path'#39's Units'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object chkSearchProjectFiles: TCheckBox
      Left = 19
      Top = 22
      Width = 159
      Height = 17
      Caption = 'Search in Project Units'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object chkDelphiFiles: TCheckBox
      Left = 193
      Top = 21
      Width = 163
      Height = 17
      Caption = 'Search in Delphi Files'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
  end
  object grpResult: TGroupBox
    Left = 0
    Top = 54
    Width = 359
    Height = 247
    Align = alClient
    Caption = 'Result'
    TabOrder = 1
    object lstResult: TListBox
      Left = 2
      Top = 15
      Width = 355
      Height = 169
      Align = alClient
      ItemHeight = 13
      Items.Strings = (
        'asfsafsad'
        'ff'
        'fsadf'
        'asf'
        'asd'
        'fas'
        'f'
        'asd'
        'fsad'
        'f'
        'sad'
        'f'
        'asdf')
      TabOrder = 0
      OnKeyPress = lstResultKeyPress
    end
    object pnlResultBottom: TPanel
      Left = 2
      Top = 184
      Width = 355
      Height = 61
      Align = alBottom
      BevelOuter = bvNone
      BiDiMode = bdLeftToRight
      ParentBiDiMode = False
      TabOrder = 1
      object btnAdd: TButton
        Left = 213
        Top = 19
        Width = 110
        Height = 25
        Caption = 'Add'
        TabOrder = 1
        OnClick = btnAddClick
      end
      object rgUsesAddTo: TRadioGroup
        Left = 0
        Top = 0
        Width = 164
        Height = 61
        Margins.Top = 10
        Align = alLeft
        Caption = 'Add To'
        ItemIndex = 0
        Items.Strings = (
          'Interface'
          'Implementation')
        TabOrder = 0
      end
    end
  end
  object grpSearch: TGroupBox
    Left = 0
    Top = 0
    Width = 359
    Height = 54
    Align = alTop
    Caption = 'Search'
    TabOrder = 0
    object edtSearch: TEdit
      Left = 19
      Top = 18
      Width = 330
      Height = 21
      TabOrder = 0
      Text = 'edtSearch'
      OnChange = edtSearchChange
      OnKeyDown = edtSearchKeyDown
    end
  end
end
