object frmFindUnit: TfrmFindUnit
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'Find Uses'
  ClientHeight = 670
  ClientWidth = 803
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  Visible = True
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 130
  TextHeight = 18
  object grpOptions: TGroupBox
    Left = 0
    Top = 575
    Width = 803
    Height = 95
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alBottom
    Caption = 'Search Options'
    TabOrder = 2
    object lblProjectUnitsStatus: TLabel
      Left = 267
      Top = 32
      Width = 64
      Height = 18
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Caption = 'Loading...'
    end
    object lblLibraryUnitsStatus: TLabel
      Left = 267
      Top = 64
      Width = 64
      Height = 18
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Caption = 'Loading...'
    end
    object btnRefreshProject: TSpeedButton
      Left = 260
      Top = 24
      Width = 75
      Height = 30
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = '&Refresh'
      Visible = False
      OnClick = btnRefreshProjectClick
    end
    object btnRefreshLibraryPath: TSpeedButton
      Left = 260
      Top = 55
      Width = 75
      Height = 31
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Re&fresh'
      Visible = False
      OnClick = btnRefreshLibraryPathClick
    end
    object chkSearchLibraryPath: TCheckBox
      Left = 26
      Top = 62
      Width = 226
      Height = 24
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Search in Library Path'#39's Units'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = chkSearchLibraryPathClick
    end
    object chkSearchProjectFiles: TCheckBox
      Left = 26
      Top = 30
      Width = 220
      Height = 24
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Search in Project Units'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = chkSearchProjectFilesClick
    end
  end
  object grpResult: TGroupBox
    Left = 0
    Top = 101
    Width = 803
    Height = 474
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alClient
    Caption = 'Result'
    TabOrder = 1
    object lstResult: TListBox
      Left = 2
      Top = 20
      Width = 799
      Height = 452
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alClient
      ItemHeight = 18
      TabOrder = 0
      OnDblClick = lstResultDblClick
    end
  end
  object grpSearch: TGroupBox
    Left = 0
    Top = 0
    Width = 803
    Height = 101
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alTop
    Caption = 'Search'
    TabOrder = 0
    object lblWhere: TLabel
      Left = 22
      Top = 64
      Width = 43
      Height = 18
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Caption = 'Add to'
    end
    object edtSearch: TEdit
      Left = 22
      Top = 25
      Width = 457
      Height = 26
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      TabOrder = 0
      Text = 'Type your search...'
      OnChange = edtSearchChange
      OnClick = edtSearchClick
      OnKeyDown = edtSearchKeyDown
    end
    object rbInterface: TRadioButton
      Left = 116
      Top = 62
      Width = 157
      Height = 24
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = '&Interface'
      Checked = True
      TabOrder = 1
      TabStop = True
    end
    object rbImplementation: TRadioButton
      Left = 281
      Top = 62
      Width = 157
      Height = 24
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Im&plementation'
      TabOrder = 2
    end
    object btnAdd: TButton
      Left = 658
      Top = 33
      Width = 117
      Height = 35
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = '&Add'
      TabOrder = 3
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
    Top = 427
  end
end
