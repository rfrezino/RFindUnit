object frmFindUnit: TfrmFindUnit
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'Find Uses'
  ClientHeight = 484
  ClientWidth = 580
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
    Top = 415
    Width = 580
    Height = 69
    Align = alBottom
    Caption = 'Search Options'
    TabOrder = 2
    object lblProjectUnitsStatus: TLabel
      Left = 193
      Top = 23
      Width = 49
      Height = 13
      Caption = 'Loading...'
    end
    object lblLibraryUnitsStatus: TLabel
      Left = 193
      Top = 46
      Width = 49
      Height = 13
      Caption = 'Loading...'
    end
    object btnRefreshProject: TSpeedButton
      Left = 188
      Top = 17
      Width = 54
      Height = 22
      Caption = '&Refresh'
      Visible = False
      OnClick = btnRefreshProjectClick
    end
    object btnRefreshLibraryPath: TSpeedButton
      Left = 188
      Top = 40
      Width = 54
      Height = 22
      Caption = 'Re&fresh'
      Visible = False
      OnClick = btnRefreshLibraryPathClick
    end
    object chkSearchLibraryPath: TCheckBox
      Left = 19
      Top = 45
      Width = 163
      Height = 17
      Caption = 'Search in Library Path'#39's Units'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = chkSearchLibraryPathClick
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
      OnClick = chkSearchProjectFilesClick
    end
  end
  object grpResult: TGroupBox
    Left = 0
    Top = 73
    Width = 580
    Height = 342
    Align = alClient
    Caption = 'Result'
    TabOrder = 1
    object lstResult: TListBox
      Left = 2
      Top = 15
      Width = 576
      Height = 325
      Align = alClient
      ItemHeight = 13
      TabOrder = 0
      OnDblClick = lstResultDblClick
    end
  end
  object grpSearch: TGroupBox
    Left = 0
    Top = 0
    Width = 580
    Height = 73
    Align = alTop
    Caption = 'Search'
    TabOrder = 0
    object lblWhere: TLabel
      Left = 16
      Top = 46
      Width = 32
      Height = 13
      Caption = 'Add to'
    end
    object edtSearch: TEdit
      Left = 16
      Top = 18
      Width = 330
      Height = 21
      TabOrder = 0
      Text = 'Type your search...'
      OnChange = edtSearchChange
      OnClick = edtSearchClick
      OnKeyDown = edtSearchKeyDown
    end
    object rbInterface: TRadioButton
      Left = 84
      Top = 45
      Width = 113
      Height = 17
      Caption = '&Interface'
      Checked = True
      TabOrder = 2
      TabStop = True
    end
    object rbImplementation: TRadioButton
      Left = 203
      Top = 45
      Width = 113
      Height = 17
      Caption = 'Im&plementation'
      TabOrder = 3
    end
    object btnAdd: TButton
      Left = 475
      Top = 24
      Width = 85
      Height = 25
      Caption = '&Add'
      TabOrder = 1
      OnClick = btnAddClick
    end
  end
  object aevKeys: TApplicationEvents
    OnMessage = aevKeysMessage
    Left = 537
    Top = 423
  end
  object tmrLoadedItens: TTimer
    Interval = 700
    OnTimer = tmrLoadedItensTimer
    Left = 470
    Top = 428
  end
end
