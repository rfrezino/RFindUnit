object frmSettings: TfrmSettings
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'Settings'
  ClientHeight = 437
  ClientWidth = 654
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pgcMain: TPageControl
    Left = 0
    Top = 0
    Width = 654
    Height = 437
    ActivePage = tsGeneral
    Align = alClient
    TabOrder = 0
    object tsGeneral: TTabSheet
      Caption = 'General'
      ImageIndex = 1
      object grpGeneralSettings: TGroupBox
        Left = 0
        Top = 0
        Width = 646
        Height = 409
        Align = alClient
        Caption = 'Settings'
        TabOrder = 0
        object chkAlwaysImportToInterfaceSection: TCheckBox
          Left = 15
          Top = 20
          Width = 229
          Height = 17
          Caption = 'Always import to interface section'
          TabOrder = 0
        end
        object chkMemorize: TCheckBox
          Left = 15
          Top = 43
          Width = 274
          Height = 17
          Caption = 'Store choices to use on Auto Import (Ctrl + Shit + I)'
          Checked = True
          State = cbChecked
          TabOrder = 1
        end
        object chkBreakline: TCheckBox
          Left = 15
          Top = 87
          Width = 210
          Height = 17
          Caption = 'Break line at each new uses entry'
          TabOrder = 3
        end
        object chkSortAfterAdding: TCheckBox
          Left = 15
          Top = 65
          Width = 210
          Height = 17
          Caption = 'Sort uses after inserting'
          TabOrder = 2
          OnClick = chkSortAfterAddingClick
        end
        object grpSearchAlgorithm: TRadioGroup
          Left = 15
          Top = 152
          Width = 167
          Height = 81
          Caption = 'Search match algorithm '
          Items.Strings = (
            'Default'
            'Levenshtein')
          TabOrder = 5
        end
        object chkBlankLineBtwNamespace: TCheckBox
          Left = 32
          Top = 109
          Width = 193
          Height = 17
          Caption = 'Blank line between namescapes'
          TabOrder = 4
        end
      end
    end
    object tsAutoImport: TTabSheet
      Caption = 'Auto Import'
      object grdAutoImport: TDBGrid
        Left = 0
        Top = 48
        Width = 646
        Height = 336
        Align = alClient
        DataSource = dtsAutoImport
        Options = [dgEditing, dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgTitleClick, dgTitleHotTrack]
        TabOrder = 1
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
        Columns = <
          item
            Expanded = False
            FieldName = 'IDENTIFIER'
            Width = 300
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'UNIT'
            Width = 300
            Visible = True
          end>
      end
      object grpAutoSettings: TGroupBox
        Left = 0
        Top = 0
        Width = 646
        Height = 48
        Align = alTop
        Caption = 'Settings'
        TabOrder = 0
        object chkAutoEnabled: TCheckBox
          Left = 16
          Top = 18
          Width = 65
          Height = 21
          Caption = 'Enabled'
          TabOrder = 1
        end
        object btn1: TButton
          Left = 512
          Top = 14
          Width = 125
          Height = 25
          Caption = 'Open Settings File'
          TabOrder = 0
          OnClick = btn1Click
        end
      end
      object nvgAutoImport: TDBNavigator
        Left = 0
        Top = 384
        Width = 646
        Height = 25
        DataSource = dtsAutoImport
        Align = alBottom
        ConfirmDelete = False
        TabOrder = 2
      end
    end
  end
  object cdsAutoImport: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 519
    Top = 353
    object cdsAutoImportIDENTIFIER: TStringField
      FieldName = 'IDENTIFIER'
      Size = 200
    end
    object cdsAutoImportUNIT: TStringField
      FieldName = 'UNIT'
      Size = 200
    end
  end
  object dtsAutoImport: TDataSource
    DataSet = cdsAutoImport
    Left = 596
    Top = 356
  end
end
