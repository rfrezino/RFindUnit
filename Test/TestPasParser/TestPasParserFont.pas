unit TestPasParserFont;

interface

const
  TEST_CONT = 'test const';

type
  TTestShortCut = Low(Word)..High(Word);

  TObjectConverterAv = reference to function(Data: TObject; Field: string): TObject;

  TProcedureTestEvent = procedure(Sender: TObject; FunctionResult: String; var Feedback: String) of object;
  TFunctionTestEvent = function(Sender: TObject; FunctionResult: String; var Feedback: String): string of object;


  TestEnum = (teOne, teTwo, teThree);
  TestSet = set of TestEnum;
  TestSetNew = set of 1..10;

  TVATID = type string;
  TTESTNotIn = type string;

  TVATIDHelper = record helper for TVATID
  end;

  TExtraInfo = class helper for TObject
  end;

  IOtherInterface = interface
    ['{9B20D6D4-0E6C-44B6-A8D6-CF1DC3ACFE55}']
  end;

  TRecordWin = record
    TestValue: string;
  end;

  TTest1 = class(TObject)
  public
    procedure NoShow;
    class procedure YesShow;

    function NoShowFunction: string;
    class function YesShowFunction: string;

    constructor Create;
    destructor Destroy; override;
  end;

  TFowarededType = TTest1;

  TDescendantClass = class(TTest1)
  public
    class function DescendantClassShow: Integer;
  end;

  procedure YesOutProcedure;

const
  TEST_CONT_BREAK = 'test const';

type
  TestEnumValue = (tevOne = 1, tevTwo = 2, tevThree = 3);

  TTest2 = class(TObject)
  public
    procedure NoShow;
    class procedure YesShow;

    function NoShowFunction: string;
    class function YesShowFunction: string;

    constructor Create;
    destructor Destroy; override;
  end;

  function YesOutFunction: string;

implementation

procedure YesOutProcedure;
begin

end;

function YesOutFunction: string;
begin

end;

{ TTest1 }

constructor TTest1.Create;
begin

end;

destructor TTest1.Destroy;
begin

  inherited;
end;

procedure TTest1.NoShow;
begin

end;

function TTest1.NoShowFunction: string;
begin

end;

class procedure TTest1.YesShow;
begin

end;

class function TTest1.YesShowFunction: string;
begin

end;

{ TTest2 }

constructor TTest2.Create;
begin

end;

destructor TTest2.Destroy;
begin

  inherited;
end;

procedure TTest2.NoShow;
begin

end;

function TTest2.NoShowFunction: string;
begin

end;

class procedure TTest2.YesShow;
begin

end;

class function TTest2.YesShowFunction: string;
begin

end;

{ TDescendantClass }

class function TDescendantClass.DescendantClassShow: Integer;
begin

end;

end.
