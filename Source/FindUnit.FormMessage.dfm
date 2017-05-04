object frmMessage: TfrmMessage
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'Message'
  ClientHeight = 56
  ClientWidth = 629
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object tmrClose: TTimer
    Enabled = False
    Interval = 2500
    OnTimer = tmrCloseTimer
    Left = 530
    Top = 15
  end
end
