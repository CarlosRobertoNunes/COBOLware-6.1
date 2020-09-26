unit UnEmail;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Mask, wwdbedit, Buttons,IniFiles,
  DBCtrls, DFEdit;

type
  TFrmEmail = class(TForm)
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Panel2: TPanel;
    BtnFechar: TBitBtn;
    BtnSalvar: TBitBtn;
    CpoRequerSSL: TCheckBox;
    CpoRequerAutenticacao: TCheckBox;
    CpoSMTP: TEdit;
    CpoDestinatario: TEdit;
    CpoUsuario: TEdit;
    CpoSenha: TEdit;
    CpoPorta: TEdit;
    Label1: TLabel;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BtnFecharClick(Sender: TObject);
    procedure BtnSalvarClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmEmail: TFrmEmail;

implementation

uses UnMenu;

{$R *.dfm}

procedure TFrmEmail.FormShow(Sender: TObject);
begin
  FrmEmail.Caption:= sTituloSistema;

  CpoSMTP.Text := sServidor;
  CpoDestinatario.Text:=sEnderecoEmail;
  CpoUsuario.Text:=sUsuario;
  CpoSenha.Text:=sSenha;
  
  if AnsiUpperCase(sRequerSSL) = AnsiUpperCase('YES') then
    CpoRequerSSL.Checked := True
  else
    CpoRequerSSL.Checked := False;

  if AnsiUpperCase(sRequerAutenticacao) = AnsiUpperCase('YES') then
    CpoRequerAutenticacao.Checked := True
  else
    CpoRequerAutenticacao.Checked := False;

  CpoPorta.Text := sSMTPPort;

  CpoSMTP.SetFocus;
end;

procedure TFrmEmail.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:=Cafree;
end;

procedure TFrmEmail.BtnFecharClick(Sender: TObject);
begin
  Close;
end;

procedure TFrmEmail.BtnSalvarClick(Sender: TObject);
begin
  if Not(Trim(CpoSMTP.Text) = '' ) then
    sServidor := CpoSMTP.Text;
    
  sEnderecoEmail := CpoDestinatario.Text;
  sUsuario   := CpoUsuario.Text;
  sSenha     := CpoSenha.Text;
  sSMTPPort  := CpoPorta.Text;

  if (CpoRequerSSL.Checked) then sRequerSSL := 'YES'
  else sRequerSSL := 'NO'; 

  if (CpoRequerAutenticacao.Checked) then sRequerAutenticacao := 'YES'
  else sRequerAutenticacao := 'NO';
  
  Reenviar   := True;
  Close;
end;

end.
