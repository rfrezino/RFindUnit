unit FindUnit.FormSettings;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls;

type
  TfrmSettings = class(TForm)
    pgcMain: TPageControl;
    tsAutoImport: TTabSheet;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmSettings: TfrmSettings;

implementation

{$R *.dfm}

end.
