unit UnSobre;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls,UrlMon, jpeg;

type
  TFrmSobre = class(TForm)
    Label2: TLabel;
    Label8: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label7: TLabel;
    Label6: TLabel;
    BtnFechar: TBitBtn;
    LogoDigifred: TImage;
    procedure BtnFecharClick(Sender: TObject);
    procedure Label8Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmSobre: TFrmSobre;

implementation

uses UnMenu;

{$R *.dfm}

procedure TFrmSobre.BtnFecharClick(Sender: TObject);
begin
  close;
end;

procedure TFrmSobre.Label8Click(Sender: TObject);
begin
  HlinkNavigateString(nil,'http://www.digifred.com.br');
end;

procedure TFrmSobre.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:=CaFree;
end;

procedure TFrmSobre.FormShow(Sender: TObject);
begin
  FrmSobre.Caption:=sTituloSistema;
end;

end.
