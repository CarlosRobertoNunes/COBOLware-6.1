object FrmPrintFile: TFrmPrintFile
  Left = 370
  Top = 262
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Poster 1.2'
  ClientHeight = 264
  ClientWidth = 426
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  Visible = True
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label5: TLabel
    Left = 8
    Top = 8
    Width = 92
    Height = 13
    Caption = 'Formato v'#225'lido: '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
  end
  object LbFormato: TLabel
    Left = 100
    Top = 8
    Width = 97
    Height = 13
    Caption = 'SmartMail arquivo.ini'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object Label1: TLabel
    Left = 8
    Top = 232
    Width = 244
    Height = 26
    Caption = 'COBOLware 6.0 '#13#10'Copyright(C) 1984-2018 COBOLware Services Ltda.'
  end
  object RichEdit1: TRichEdit
    Left = 224
    Top = 8
    Width = 377
    Height = 17
    TabStop = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Terminal'
    Font.Style = []
    Lines.Strings = (
      'RichEdit1')
    ParentFont = False
    TabOrder = 0
    Visible = False
  end
  object Memo1: TMemo
    Left = 8
    Top = 27
    Width = 409
    Height = 193
    TabStop = False
    Lines.Strings = (
      '[Config]'
      'InFile=Arquivo de entrada (Formato texto)'
      
        'OutFile=Arquivo de saida, PRINTER, VIEW ou e-mail do destinat'#225'ri' +
        'o'
      'OutputFormat=DOC,HTML,PDF ou RTF'
      'Font=Fonte de OutFile'
      'Title=T'#237'tulo da Janela de progresso'
      'Quiet=Yes N'#227'o exibir telas ilustrativas'
      'PrinterMode=Graphic/Text'
      'Spacing= 6 ou 8 - Espa'#231'amento entre linhas'
      
        'PageHeight=Quantidade de linhas por p'#225'gina, quando PrinterMode=T' +
        'ext'
      'FontSize=Tamanho da Fonte'
      ''
      'Sender=E-mail do Remetente'
      'Subject=Assunto do e-mail'
      'Text=Arquivo texto para o corpo do e-mail'
      'Attach=Lista de at'#233' 10 arquivos anexos adicionais'
      'EraseAttachs=Yes - Remove os arquivos anexados ao e-mail'
      
        'EraseText=Yes - Remove o arquivo texto usado para o corpo do e-m' +
        'ail'
      ''
      'SmtpServer=Servidor de envio de mensagens'
      'User=Usu'#225'rio do SmtpServer'
      'Password=Senha do Usu'#225'rio do SmtpServer'
      'SSL=Yes Utiliza Conex'#227'o Segura'
      'Authentication=Yes Realiza Autentica'#231#227'o no Servidor'
      'QuietWhenSendError=Yes Desabilita corre'#231#245'es de SMTP'
      'SmtpPort=Porta do Servidor de envio de mensagens'
      ''
      'KeepInFile=Yes Preserva InFile ap'#243's converter'
      'KeepOutFileWhenEmail=Yes Preserva OutFile quando e-mail')
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object BtnFechar: TBitBtn
    Left = 344
    Top = 232
    Width = 73
    Height = 25
    Caption = '&Fechar   '
    TabOrder = 2
    OnClick = BtnFecharClick
    Glyph.Data = {
      C6070000424DC607000000000000360000002800000016000000160000000100
      2000000000009007000000000000000000000000000000000000FFFFFFFFF2F2
      F2FFF2F2F2F2D3D3D3F2E8E8E8D3F5F5F5E8DBDBDBF5DDDDDDDBECECECDDEEEE
      EEECD3D3D3EEEBEBEBD3F7F7F7EB23230FF74F4F0023B2B2A800E9E9E9B2D6D6
      D6E9EBEBEBD6FAFAFAEBFFFFFFFAFFFFFFFFFFFFFFD3F0F0F0FFD8D8D8F0EDED
      EDD8EAEAEAEDDFDFDFEADCDCDCDFF4F4F4DCE6E6E6F4D7D7D7E6F2F2F2D7E7E7
      E7F2DDDDDDE74C4C0CDDFFFF004C686811009B9B9B68F4F4F49BE7E7E7F4DBDB
      DBE7E6E6E6DBFFFFFFE6D3D3D3F3E4E4E4D3EFEFEFE4E4E4E4EFD8D8D8E4EDED
      EDD8F3F3F3EDD3D3D3F3E7E7E7D3F1F1F1E7DFDFDFF1DBDBDBDFECECECDB4D4D
      0DECFFFF004D8D8D00005151058D87878105DFDFDF87EDEDEDDFECECECEDD3D3
      D3ECE6E6E64EEDEDEDE6E1E1E1EDDBDBDBE1F5F5F5DBE3E3E3F5DCDCDCE3EAEA
      EADCEAEAEAEAE0E0E0EADBDBDBE0F6F6F6DBE3E3E3F64D4D0DE3FFFF004D8F8F
      00007F7F008F56560D00A1A1A156E2E2E2A1DBDBDBE2F3F3F3DB4B4B4BBF5050
      504B4E4E4E504848484E4F4F4F485050504F4A4A4A504C4C4C4A4444444CF0F0
      F044F3F3F3F0D3D3D3F3E7E7E7D34E4E0EE7FFFF004E8F8F00007F7F008F7D7D
      00004C4C0D7D3232320D484848324E4E4E48BFBFBFBFBFBFBFBFBFBFBFBFBFBF
      BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF4A4A4ABFDFDFDF4ADFDFDFDFE9E9
      E9DFEBEBEBE94D4D0DEBFFFF004D8F8F00007F7F008F7F7F00004747007F6363
      6300BFBFBF63BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
      BFBFB0B0B0BFBFBFBFB0404040BF3F3F3F403737373F373737373F3F3F374343
      033FFFFF00438F8F00007F7F008F7F7F00004747007F63636300BFBFBF63BFBF
      BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF3D3D3DBF8F8F
      8F3D3E3E3E8F7F7F7F3E7F7F7F7F7F7F7F7F7F7F7F7F4747077FFFFF00478F8F
      00007F7F008F7F7F00004747007F63636300BFBFBF63BFBFBFBFBFBFBFBFBFBF
      BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF3C3C30BF4D4D14301515134D7F7F
      7F137F7F7F7F7F7F7F7F7F7F7F7F4747077FFFFF00478D8D00007777008D7D7D
      00004747007D63636300BFBFBF63BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
      BFBFBFBFBFBFBFBFBFBF464630BF9A9A00304545049A616161047F7F7F617F7F
      7F7F7F7F7F7F4747077FFFFF00475F5F08001717175F67670017474700676363
      6300BFBFBF63BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
      BFBF474727BFEDED0027929200ED45450E00636363457F7F7F637F7F7F7F4747
      077FFFFF0047474727006B6B6B476767006B4747006763636300BFBFBF63BFBF
      BFBFAEAEAEBF575745AE31310A572727000A272700272727000042420027FFFF
      0000F9F900FF9898000042420098636363007F7F7F634747077FFFFF00477E7E
      00002727007E6E6E00004747006E63636300BFBFBF63BFBFBFBF898989BF8C8C
      1389CFCF018CCFCF0001CFCF00CFCFCF0000D5D500CFFFFF0000FFFF00FFEDED
      0000989800ED45450E006161614547470761FFFF00478F8F00007F7F008F7F7F
      00004747007F63636300BFBFBF63BFBFBFBF898989BFA1A11389FEFE2CA1FFFF
      AF2CFFFFAFFFFFFFAFAFFFFFAFFFFFFFAFAFFFFFDBFFFFFF6DDBE3E300FF4B4B
      09005B5B5B4B4747075BFFFF00478F8F00007F7F008F7F7F00004747007F6363
      6300BFBFBF63BFBFBFBFA7A7A7BF6F6F48A779790C6F7A7A0A0C7A7A0A7A7A7A
      0A0A8A8A097AFFFF3E09FFFF70FFE4E400705F5F00E4585858007F7F7F584747
      077FFFFF00478F8F00007F7F008F7F7F00004747007F63636300BFBFBF63BFBF
      BFBFBFBFBFBFB1B1B1BFA9A9A9B1A9A9A9A9A9A9A9A9A9A9A9A94C4C2CA9FFFF
      0D2CE6E607FF59590807585853597F7F7F537F7F7F7F4747077FFFFF00478F8F
      00007F7F008F7F7F00004747007F63636300BFBFBF63BFBFBFBFBFBFBFBFBFBF
      BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF505030BFE3E300305F5F03E35757
      57037F7F7F577F7F7F7F7F7F7F7F4747077FFFFF00478F8F00007F7F008F7F7F
      00004747007F63636300BFBFBF63BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
      BFBFBFBFBFBFBFBFBFBF424235BF6E6E253521211B6E7F7F7F1B7F7F7F7F7F7F
      7F7F7F7F7F7F5E5E527F9999065ED5D50006878700D57F7F00004747007F6363
      6300BFBFBF63BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
      BFBF707070BF88888870444444887F7F7F447F7F7F7F7F7F7F7F7F7F7F7F7F7F
      7F7F5656567F99990656DBDB0099898900004747008963636300BFBFBF63BFBF
      BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
      BFBF444444BF7F7F7F447F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F5E5E
      527F9B9B035ED7D700034C4C00D776767600BFBFBF76BFBFBFBFBFBFBFBFBFBF
      BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF414141BF5656
      56415656565656565656565656565656565656565656565656562E2E2E568888
      072E65650C888888880CBFBFBF88BFBFBFBFBFBFBF00BFBFBFBFBFBFBFBFBFBF
      BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF969696BF89898996898989898989
      898989898989898989898989898989898989898989898989898989898989B1B1
      B189BFBFBFB1BFBFBFBF}
  end
  object ZReport1: TZReport
    Left = 392
    Top = -8
    Width = 220
    Height = 66
    DataSet = ClientDataSet1
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Options.LineSpacing = zrd16
    Options.AutoHeight = False
    Options.PageFrom = 1
    Options.PageTo = 1
    Options.Acentos = Padrao
    Options.Scissors = False
    Margins.Left = 1
    PrintIfEmpty = True
    Minimized = True
    object zrvLinha: TZRField
      Format.Width = 300
      DataField = 'Linha'
      DataSet = ClientDataSet1
    end
    object zrvidx: TZRField
      Format.Width = 6
      DataField = 'idx'
      DataSet = ClientDataSet1
    end
    object ZReport1Group: TZRGroup
      Variable = zrvidx
    end
    object ZReport1Detail: TZRBand
      Left = 1
      Top = 11
      Width = 218
      Height = 1
      Stretch = False
      BandType = zbtDetail
      object ZRLabel1: TZRLabel
        Left = 0
        Top = 0
        Width = 220
        Height = 1
        AutoSize = zasHeight
        Caption = 'ZRLabel1'
        Variable = zrvLinha
      end
    end
  end
  object Memo2: TMemo
    Left = 360
    Top = 112
    Width = 33
    Height = 33
    TabStop = False
    TabOrder = 4
    Visible = False
  end
  object RichEdit2: TRichEdit
    Left = 344
    Top = 168
    Width = 49
    Height = 33
    TabStop = False
    TabOrder = 5
    Visible = False
  end
  object ClientDataSet1: TClientDataSet
    Active = True
    Aggregates = <>
    Params = <>
    Left = 304
    Top = 24
    Data = {
      400000009619E0BD010000001800000002000000000003000000400003696478
      0400010000000000054C696E6861020049000000010005574944544802000200
      2C010000}
    object ClientDataSet1idx: TIntegerField
      DisplayWidth = 6
      FieldName = 'idx'
    end
    object ClientDataSet1Linha: TStringField
      FieldName = 'Linha'
      Size = 300
    end
  end
  object ppDBPipeline1: TppDBPipeline
    DataSource = DataSource1
    UserName = 'DBPipeline1'
    OnNext = ppDBPipeline1Next
    Left = 272
    Top = 23
    object ppDBPipeline1ppField1: TppField
      Alignment = taRightJustify
      FieldAlias = 'idx'
      FieldName = 'idx'
      FieldLength = 0
      DataType = dtInteger
      DisplayWidth = 6
      Position = 0
    end
    object ppDBPipeline1ppField2: TppField
      FieldAlias = 'Linha'
      FieldName = 'Linha'
      FieldLength = 300
      DisplayWidth = 300
      Position = 1
    end
  end
  object MainMenu1: TMainMenu
    Left = 48
    Top = 16
    object Arquivo1: TMenuItem
      Caption = 'Arquivo'
      object Localizar1: TMenuItem
        Caption = 'Localizar'
        OnClick = Localizar1Click
      end
    end
    object Sobre1: TMenuItem
      Caption = 'Sobre'
      OnClick = Sobre1Click
    end
  end
  object DataSource1: TDataSource
    DataSet = ClientDataSet1
    Left = 240
    Top = 24
  end
  object ppReport80_132: TppReport
    AutoStop = False
    DataPipeline = ppDBPipeline1
    PrinterSetup.BinName = 'Default'
    PrinterSetup.DocumentName = 'Report'
    PrinterSetup.Duplex = dpNone
    PrinterSetup.PaperName = 'A4'
    PrinterSetup.PrinterName = 'Default'
    PrinterSetup.SaveDeviceSettings = False
    PrinterSetup.mmMarginBottom = 6350
    PrinterSetup.mmMarginLeft = 6350
    PrinterSetup.mmMarginRight = 6350
    PrinterSetup.mmMarginTop = 6350
    PrinterSetup.mmPaperHeight = 297000
    PrinterSetup.mmPaperWidth = 210000
    PrinterSetup.PaperSize = 9
    Units = utMillimeters
    AllowPrintToFile = True
    ArchiveFileName = '($MyDocuments)\ReportArchive.raf'
    DeviceType = 'PDFFile'
    DefaultFileDeviceType = 'PDF'
    EmailSettings.ReportFormat = 'PDF'
    LanguageID = 'Default'
    ModalCancelDialog = False
    ModalPreview = False
    OpenFile = False
    OutlineSettings.CreateNode = False
    OutlineSettings.CreatePageNodes = False
    OutlineSettings.Enabled = False
    OutlineSettings.Visible = False
    ThumbnailSettings.Enabled = True
    ThumbnailSettings.Visible = True
    ThumbnailSettings.DeadSpace = 30
    ThumbnailSettings.PageHighlight.Width = 3
    PDFSettings.EmbedFontOptions = [efUseSubset]
    PDFSettings.EncryptSettings.AllowCopy = True
    PDFSettings.EncryptSettings.AllowInteract = True
    PDFSettings.EncryptSettings.AllowModify = True
    PDFSettings.EncryptSettings.AllowPrint = True
    PDFSettings.EncryptSettings.AllowExtract = True
    PDFSettings.EncryptSettings.AllowAssemble = True
    PDFSettings.EncryptSettings.AllowQualityPrint = True
    PDFSettings.EncryptSettings.Enabled = False
    PDFSettings.EncryptSettings.KeyLength = kl40Bit
    PDFSettings.EncryptSettings.EncryptionType = etRC4
    PDFSettings.FontEncoding = feAnsi
    PDFSettings.ImageCompressionLevel = 25
    PDFSettings.PDFAFormat = pafNone
    PreviewFormSettings.PageBorder.mmPadding = 0
    PreviewFormSettings.WindowState = wsMaximized
    PreviewFormSettings.ZoomSetting = zsPageWidth
    RTFSettings.DefaultFont.Charset = DEFAULT_CHARSET
    RTFSettings.DefaultFont.Color = clWindowText
    RTFSettings.DefaultFont.Height = -13
    RTFSettings.DefaultFont.Name = 'Arial'
    RTFSettings.DefaultFont.Style = []
    ShowCancelDialog = False
    ShowPrintDialog = False
    TextFileName = '($MyDocuments)\Report.pdf'
    TextSearchSettings.DefaultString = '<FindText>'
    TextSearchSettings.Enabled = False
    XLSSettings.AppName = 'ReportBuilder'
    XLSSettings.Author = 'ReportBuilder'
    XLSSettings.Subject = 'Report'
    XLSSettings.Title = 'Report'
    XLSSettings.WorksheetName = 'Report'
    Left = 166
    Top = 6
    Version = '19.0'
    mmColumnWidth = 197300
    DataPipelineName = 'ppDBPipeline1'
    object ppDetailBand3: TppDetailBand
      Background1.Brush.Style = bsClear
      Background2.Brush.Style = bsClear
      Border.mmPadding = 0
      mmBottomOffset = 0
      mmHeight = 5080
      mmPrintPosition = 0
      object ppLinhaRelatorio: TppDBText
        DesignLayer = ppDesignLayer1
        UserName = 'LinhaRelatorio'
        HyperlinkEnabled = False
        RTLReading = False
        Border.mmPadding = 0
        DataField = 'Linha'
        DataPipeline = ppDBPipeline1
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Name = 'Courier New'
        Font.Size = 8
        Font.Style = []
        Transparent = True
        DataPipelineName = 'ppDBPipeline1'
        mmHeight = 3704
        mmLeft = 0
        mmTop = 529
        mmWidth = 197380
        BandType = 4
        LayerName = Foreground
      end
    end
    object ppGroup3: TppGroup
      BreakName = 'idx'
      DataPipeline = ppDBPipeline1
      GroupFileSettings.NewFile = False
      GroupFileSettings.EmailFile = False
      OutlineSettings.CreateNode = True
      NewPage = True
      ReprintOnSubsequentPage = False
      StartOnOddPage = False
      UserName = 'Group3'
      mmNewColumnThreshold = 0
      mmNewPageThreshold = 0
      DataPipelineName = 'ppDBPipeline1'
      NewFile = False
      object ppGroupHeaderBand3: TppGroupHeaderBand
        Background.Brush.Style = bsClear
        Border.mmPadding = 0
        mmBottomOffset = 0
        mmHeight = 0
        mmPrintPosition = 0
      end
      object ppGroupFooterBand3: TppGroupFooterBand
        Background.Brush.Style = bsClear
        Border.mmPadding = 0
        HideWhenOneDetail = False
        mmBottomOffset = 0
        mmHeight = 0
        mmPrintPosition = 0
      end
    end
    object ppDesignLayers1: TppDesignLayers
      object ppDesignLayer1: TppDesignLayer
        UserName = 'Foreground'
        LayerType = ltBanded
        Index = 0
      end
    end
    object ppParameterList1: TppParameterList
    end
  end
  object ppReport220: TppReport
    AutoStop = False
    DataPipeline = ppDBPipeline1
    PrinterSetup.BinName = 'Default'
    PrinterSetup.DocumentName = 'Report'
    PrinterSetup.Duplex = dpNone
    PrinterSetup.Orientation = poLandscape
    PrinterSetup.PaperName = 'A4'
    PrinterSetup.PrinterName = 'Default'
    PrinterSetup.SaveDeviceSettings = False
    PrinterSetup.mmMarginBottom = 6000
    PrinterSetup.mmMarginLeft = 5000
    PrinterSetup.mmMarginRight = 0
    PrinterSetup.mmMarginTop = 6000
    PrinterSetup.mmPaperHeight = 210000
    PrinterSetup.mmPaperWidth = 297000
    PrinterSetup.PaperSize = 9
    Units = utMillimeters
    AllowPrintToFile = True
    ArchiveFileName = '($MyDocuments)\ReportArchive.raf'
    DeviceType = 'PDFFile'
    DefaultFileDeviceType = 'PDF'
    EmailSettings.ReportFormat = 'PDF'
    LanguageID = 'Default'
    ModalCancelDialog = False
    ModalPreview = False
    OpenFile = False
    OutlineSettings.CreateNode = False
    OutlineSettings.CreatePageNodes = False
    OutlineSettings.Enabled = False
    OutlineSettings.Visible = False
    ThumbnailSettings.Enabled = True
    ThumbnailSettings.Visible = True
    ThumbnailSettings.DeadSpace = 30
    ThumbnailSettings.PageHighlight.Width = 3
    PDFSettings.EmbedFontOptions = [efUseSubset]
    PDFSettings.EncryptSettings.AllowCopy = True
    PDFSettings.EncryptSettings.AllowInteract = True
    PDFSettings.EncryptSettings.AllowModify = True
    PDFSettings.EncryptSettings.AllowPrint = True
    PDFSettings.EncryptSettings.AllowExtract = True
    PDFSettings.EncryptSettings.AllowAssemble = True
    PDFSettings.EncryptSettings.AllowQualityPrint = True
    PDFSettings.EncryptSettings.Enabled = False
    PDFSettings.EncryptSettings.KeyLength = kl40Bit
    PDFSettings.EncryptSettings.EncryptionType = etRC4
    PDFSettings.FontEncoding = feAnsi
    PDFSettings.ImageCompressionLevel = 25
    PDFSettings.PDFAFormat = pafNone
    PreviewFormSettings.PageBorder.mmPadding = 0
    PreviewFormSettings.WindowState = wsMaximized
    PreviewFormSettings.ZoomSetting = zsPageWidth
    RTFSettings.DefaultFont.Charset = DEFAULT_CHARSET
    RTFSettings.DefaultFont.Color = clWindowText
    RTFSettings.DefaultFont.Height = -13
    RTFSettings.DefaultFont.Name = 'Arial'
    RTFSettings.DefaultFont.Style = []
    ShowCancelDialog = False
    ShowPrintDialog = False
    TextFileName = '($MyDocuments)\Report.pdf'
    TextSearchSettings.DefaultString = '<FindText>'
    TextSearchSettings.Enabled = False
    XLSSettings.AppName = 'ReportBuilder'
    XLSSettings.Author = 'ReportBuilder'
    XLSSettings.Subject = 'Report'
    XLSSettings.Title = 'Report'
    XLSSettings.WorksheetName = 'Report'
    Left = 197
    Top = 6
    Version = '19.0'
    mmColumnWidth = 0
    DataPipelineName = 'ppDBPipeline1'
    object ppDetailBand1: TppDetailBand
      Background1.Brush.Style = bsClear
      Background2.Brush.Style = bsClear
      Border.mmPadding = 0
      mmBottomOffset = 0
      mmHeight = 5027
      mmPrintPosition = 0
      object ppLinhaRelatorio2: TppDBText
        DesignLayer = ppDesignLayer2
        UserName = 'LinhaRelatorio'
        HyperlinkEnabled = False
        RTLReading = False
        Border.mmPadding = 0
        DataField = 'Linha'
        DataPipeline = ppDBPipeline1
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Name = 'Courier New'
        Font.Size = 8
        Font.Style = []
        Transparent = True
        DataPipelineName = 'ppDBPipeline1'
        mmHeight = 3704
        mmLeft = 0
        mmTop = 529
        mmWidth = 283898
        BandType = 4
        LayerName = Foreground1
      end
    end
    object ppGroup1: TppGroup
      BreakName = 'idx'
      DataPipeline = ppDBPipeline1
      GroupFileSettings.NewFile = False
      GroupFileSettings.EmailFile = False
      OutlineSettings.CreateNode = True
      NewPage = True
      ReprintOnSubsequentPage = False
      StartOnOddPage = False
      UserName = 'Group3'
      mmNewColumnThreshold = 0
      mmNewPageThreshold = 0
      DataPipelineName = 'ppDBPipeline1'
      NewFile = False
      object ppGroupHeaderBand1: TppGroupHeaderBand
        Background.Brush.Style = bsClear
        Border.mmPadding = 0
        mmBottomOffset = 0
        mmHeight = 0
        mmPrintPosition = 0
      end
      object ppGroupFooterBand1: TppGroupFooterBand
        Background.Brush.Style = bsClear
        Border.mmPadding = 0
        HideWhenOneDetail = False
        mmBottomOffset = 0
        mmHeight = 0
        mmPrintPosition = 0
      end
    end
    object ppDesignLayers2: TppDesignLayers
      object ppDesignLayer2: TppDesignLayer
        UserName = 'Foreground1'
        LayerType = ltBanded
        Index = 0
      end
    end
    object ppParameterList2: TppParameterList
    end
  end
  object IdMessage1: TIdMessage
    AttachmentEncoding = 'MIME'
    BccList = <>
    CCList = <>
    Encoding = meMIME
    FromList = <
      item
      end>
    Priority = mpHigh
    Recipients = <>
    ReplyTo = <>
    ConvertPreamble = True
    Left = 312
    Top = 80
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '*.ini'
    Filter = 'Arquivo de Configura'#231#227'o (*.ini;*.txt)|*.ini;*.txt'
    Left = 360
    Top = 32
  end
  object PrinterSetupDialog1: TPrinterSetupDialog
    Left = 264
    Top = 80
  end
  object IdSSLIOHandlerSocketOpenSSL1: TIdSSLIOHandlerSocketOpenSSL
    MaxLineAction = maException
    Port = 0
    DefaultPort = 0
    SSLOptions.Method = sslvSSLv2
    SSLOptions.Mode = sslmUnassigned
    SSLOptions.VerifyMode = []
    SSLOptions.VerifyDepth = 0
    Left = 312
    Top = 107
  end
  object IdSMTP1: TIdSMTP
    OnWork = IdSMTP1Work
    SASLMechanisms = <>
    Left = 344
    Top = 80
  end
end
