object FrmConversor: TFrmConversor
  Left = 421
  Top = 401
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'SmartMail 1.1'
  ClientHeight = 122
  ClientWidth = 425
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PanelProgress: TPanel
    Left = 8
    Top = 7
    Width = 409
    Height = 106
    BorderStyle = bsSingle
    TabOrder = 0
    object Gauge1: TGauge
      Left = 11
      Top = 37
      Width = 382
      Height = 28
      ForeColor = clBlue
      Progress = 0
    end
    object LbAguarde: TLabel
      Left = 13
      Top = 15
      Width = 48
      Height = 13
      Caption = 'Aguarde'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object BtnCancelar: TBitBtn
      Left = 152
      Top = 69
      Width = 86
      Height = 25
      Caption = '&Cancelar'
      TabOrder = 0
      Visible = False
      OnClick = BtnCancelarClick
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        0400000000000001000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333333333FFFFF3333333333999993333333333F88888FFF333333999999999
        3333333888333888FF3333993333339993333388FF3333388FF3399993333339
        993338888FF3333388F3393999333333993338F888FF333338FF993399933333
        399388F3888FF333388F993339993333399388F33888FF33388F993333999333
        399388F333888FF3388F993333399933399388F3333888FF388F993333339993
        399388FF3333888FF8833993333339993933383FF3333888F8F3399933333399
        99333883FF3333888833339993333339933333883FFFFFF88333333999999999
        3333333888333888333333333999993333333333388888333333}
      NumGlyphs = 2
    end
  end
end
