unit UnConversor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, Gauges, ExtCtrls, ppDB, ppDBPipe, DB,
  DBClient,  ppBands, ppClass, ppPrnabl, ppCtrls, ppCache,
  ppComm, ppRelatv, ppProd, ppReport, Grids, DBGrids;

type
  TFrmConversor = class(TForm)
    PanelProgress: TPanel;
    Gauge1: TGauge;
    LbAguarde: TLabel;
    BtnCancelar: TBitBtn;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BtnCancelarClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  function MsgConfSalvar(Msg: String): Boolean;

var
  FrmConversor: TFrmConversor;

implementation

uses UnMenu;

{$R *.dfm}

procedure TFrmConversor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:=Cafree;
end;

procedure TFrmConversor.BtnCancelarClick(Sender: TObject);
begin
  bCancela:=MsgConfSalvar( 'Deseja realmente Cancelar a operação?');
end;

function MsgConfSalvar(Msg: String): Boolean;
begin
  case Application.MessageBox(PChar(Msg), Pchar(ExtractFileName(Application.ExeName)),
    mb_applmodal + mb_iconquestion +
    mb_yesno + mb_defbutton1) of
    IDYES: Result := True;
    IDNO: Result := False;
  end;
end;

procedure TFrmConversor.FormShow(Sender: TObject);
begin
  if Length(sAssunto) <> 0 then
    FrmConversor.Caption:= sTituloSistema + ' - ' + sAssunto
  else
    FrmConversor.Caption:= sTituloSistema + ' - ' + ExtractFileName(ArqOrigem);
end;

{

  if (copy(ArqDestino,1,3) = 'LPT') or
     (copy(ArqDestino,1,2) = '\\') then
     PortaImpressora:=ArqDestino
  else
     PortaImpressora:=Printer.Printers[Printer.PrinterIndex];

//  SizeFont=Tamanho da fonte de OutFile
//  Orientation=Orientação da impressora

  if AnsiUpperCase(sOrientation) = 'LANDSCAPE' then
    RDprint1.Orientacao:=poLandscape
  else
    RDprint1.Orientacao:=poPortrait;

  //if Length(sFontSize) <> 0 then
//    RDprint1.FonteTamanhoPadrao:=StrToIntDef(sFontSize,8);

//  RDprint1.SetPrinterbyPorta(PortaImpressora);

  if (copy(ArqDestino,1,7) = 'PREVIEW') then
    RDprint1.OpcoesPreview.Preview:=True;

  try
    RDprint1.Abrir;

    //Calcula a largura do relatório cfe. a maior linha
    SizeRel:=0;
    i:=0;
    AssignFile(TXT,ArqOrigem);
    Reset(TXT);
    while not Eof(TXT) do
    begin
      ReadLn(TXT,LinhaAux);
      inc(i);
      if i < 60 then
        if Length(TrimRight(LinhaAux)) > SizeRel then
          SizeRel:=Length(TrimRight(LinhaAux));
      inc(NroLinhasArq);
    end;
    CloseFile(TXT);

    AssignFile(TXT,ArqOrigem);
    Reset(TXT);
    iLinha:=0;
    while not Eof(TXT) do
    begin
      ReadLn(TXT,LinhaAux);
      inc(iLinha);

      for i:=1 to 10 do
      begin
        if Copy(LinhaAux,i,1) = chr(ord(#12)) then
        begin
          RDprint1.Novapagina;
          break;
        end;
      end;

      if Length(TrimRight(LinhaAux)) > SizeRel then
        RDprint1.Imp(iLinha,1,copy(ConverteStr(LinhaAux),1,sizerel - 1))
      else
        RDprint1.Imp(iLinha,1,ConverteStr(LinhaAux));

      for i:=10 to Length(LinhaAux) do
      begin
        if Copy(LinhaAux,i,1) = chr(ord(#12)) then
        begin
          if Length(TrimRight(LinhaAux)) > SizeRel then
          begin
            inc(iLinha);
            RDprint1.Imp(iLinha,1,
                         copy(ConverteStr(LinhaAux),i+1,Length(TrimRight(LinhaAux))));
          end;
          RDprint1.Novapagina;
          break;
        end;
      end;
    end;
    CloseFile(TXT);
    RDprint1.Fechar;

  except
    Application.Terminate;
    Abort;
  end;



  //rdprint


    
  if (copy(ArqDestino,1,3) = 'LPT') or
     (copy(ArqDestino,1,2) = '\\') then
     PortaImpressora:=ArqDestino
  else
     PortaImpressora:=Printer.Printers[Printer.PrinterIndex];

//  SizeFont=Tamanho da fonte de OutFile
//  Orientation=Orientação da impressora

  //if AnsiUpperCase(sOrientation) = 'LANDSCAPE' then
//    RDprint1.Orientacao:=poLandscape
//  else
//    RDprint1.Orientacao:=poPortrait;

  //if Length(sFontSize) <> 0 then
//    RDprint1.FonteTamanhoPadrao:=StrToIntDef(sFontSize,8);

//  RDprint1.SetPrinterbyPorta(PortaImpressora);

  if (copy(ArqDestino,1,7) = 'PREVIEW') then
    RDprint1.OpcoesPreview.Preview:=True;

  try
    //RDprint1.Abrir;
    //AssignFile(Impressora,PortaImpressora);
    //Rewrite(Impressora); //abre a porta da impressão

    AssignFile(TXT,ArqOrigem);
    Reset(TXT);

    Cds_Zreport.Close;
    Cds_Zreport.Open;
    Cds_Zreport.EmptyDataSet;
   // Cds_Zreport.IndexFieldNames:='LinhaAux';

    SizeRel:=0;
    NroLinhasArq:=0;
    i:=0;
    while not Eof(TXT) do
    begin
      ReadLn(TXT,LinhaAux);
      inc(i);
      if i < 60 then
        if Length(TrimRight(LinhaAux)) > SizeRel then
          SizeRel:=Length(TrimRight(LinhaAux));
      inc(NroLinhasArq);
    end;
    CloseFile(TXT);

    ZReport1.Height:=66;
    ZReport1.Width:=SizeRel + 5;
    ZRLabel1.Width:=SizeRel + 5;

    AssignFile(TXT,ArqOrigem);
    Reset(TXT);
    i:=0;
    while not Eof(TXT) do
    begin
      ReadLn(TXT,LinhaAux);
      inc(i);

      for i:=1 to 10 do
      begin
        if Copy(LinhaAux,i,1) = chr(ord(#12)) then
        begin
          RDprint1.Novapagina;
          break;
        end;
      end;

      Cds_Zreport.Append;
      if Length(TrimRight(LinhaAux)) > SizeRel then
        Cds_ZreportLinhaZR.Value:=copy(LinhaAux,1,sizerel - 1)
      else
        Cds_ZreportLinhaZR.Value:=LinhaAux;
      Cds_Zreport.Post;

      for i:=10 to Length(LinhaAux) do
      begin
        if Copy(LinhaAux,i,1) = chr(ord(#12)) then
        begin
          if Length(TrimRight(LinhaAux)) > SizeRel then
          begin
            Cds_Zreport.Append;
            Cds_ZreportLinhaZR.Value:=
              copy(LinhaAux,i+1,Length(TrimRight(LinhaAux)));
            Cds_Zreport.Post;
          end;
          break;
        end;
      end;
    end;
    CloseFile(TXT);
    ZReport1.Preview;

  except
    Application.Terminate;
    Abort;
  end;

  }

end.
