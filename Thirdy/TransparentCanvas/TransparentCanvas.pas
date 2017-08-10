//Font https://github.com/vintagedave/transparent-canvas
unit TransparentCanvas;

{
  The contents of this file are subject to the Mozilla Public License
  Version 1.1 (the "License"); you may not use this file except in
  compliance with the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS IS"
  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
  License for the specific language governing rights and limitations
  under the License.

  The Original Code is an alpha-aware canvas class and associated classes.

  The Initial Developer of the Original Code is David Millington.
  Portions created by David Millington are Copyright (C) 2008-2012.
  All Rights Reserved.

  Contributor(s): David Millington.
                  Frank Staal
}

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Controls, Vcl.Graphics;

type
  ETransparentCanvasException = class(Exception)
  end;

  TQuadColor = record
    constructor Create(Color : TColor);
    procedure Clear;
    function WrittenByGDI : Boolean;
    procedure SetAlpha(const Transparency : Byte; const PreMult : Single);
    function AsColorRef : COLORREF;
    procedure SetFromColorRef(const Color : COLORREF);
    procedure SetFromColorMultAlpha(const Color : TQuadColor);

    case Boolean of
      // These values are not in the same order as COLORREF RGB values - don't assign from a COLORREF directly
      True : (Blue,
              Green,
              Red,
              Alpha : Byte);
      False : (Quad : Cardinal);
  end;
  PQuadColor = ^TQuadColor;
  PPQuadColor = ^PQuadColor;

  TGDIObjects = record
  private
    FBrush : HBRUSH;
    FPen : HPEN;
    FFont : HFONT;
  public
    constructor CreateWithHandles(const hBrush : HBRUSH; const hPen : HPEN; const hFont : HFONT);
    property Brush : HBRUSH read FBrush;
    property Pen : HPEN read FPen;
    property Font : HFONT read FFont;
  end;

  TAlphaBitmapWrapper = class(TPersistent)
  private
    FDCHandle : HDC;
    FBMPHandle, FOriginalBMP : HBitmap;
    FQuads : PQuadColor;
    FWidth, FHeight : Integer;
    FOriginalObjects : TGDIObjects;

    procedure Construct(DC: HDC; Empty: Boolean; Width, Height: Integer; Inverted : boolean = false);
    procedure CheckHandles;
    procedure Clear;
  public
    constructor CreateBlank(DC: HDC; Width, Height: Integer);
    // The DummyX parameters are to avoid duplicate constructors with the same parameter list being
    // inaccessible from C++, although only necessary if you write C++ code that uses this (internal)
    // class
    constructor CreateForGDI(DC: HDC; Width, Height: Integer; DummyGDI : Byte = 0);
    constructor CreateForDrawThemeTextEx(DC: HDC; Width, Height: Integer; DummyDrawThemeTextEx : SmallInt = 0);
    constructor Create(var ToCopy : TAlphaBitmapWrapper);
    destructor Destroy; override;

    procedure SelectObjects(const GDI : TGDIObjects);
    procedure SelectOriginalObjects;

    procedure SetAllTransparency(const Alpha: Byte);
    procedure ProcessTransparency(const Alpha: Byte); overload;
    procedure ProcessTransparency(const Alpha: Byte; TranspRect : TRect); overload;
    procedure ProcessMaskTransparency(var MaskImage: TAlphaBitmapWrapper);
    procedure ProcessTransparentColor(const TransparentColor : COLORREF; const TransparentEdgeWidth : Integer = -1);
    procedure TintByAlphaToColor(const Color : TQuadColor);
    procedure BlendTo(X, Y: Integer; var Image: TAlphaBitmapWrapper; Transparency: Byte = $FF);
    procedure BlendToStretch(X, Y, StretchWidth, StretchHeight: Integer;
      var Image: TAlphaBitmapWrapper; Transparency: Byte);
    procedure BlendToDC(X, Y : Integer; DC : HDC; Transparency : Byte = $FF);

    function GetRawPixelPtr(const X, Y : Integer) : PQuadColor;
    procedure SafeSetRawPixel(const X, Y : Integer; Color : TQuadColor);
  published
    property Handle : HDC read FDCHandle;
    property BitmapHandle : HBitmap read FBMPHandle;
    function QuadPointer : PQuadColor;
    property Width : Integer read FWidth;
    property Height : Integer read FHeight;
  end;

  TCustomTransparentCanvas = class(TPersistent)
    class function TColorToQuadColor(Color : TColor) : TQuadColor;
    class function QuadColorToTColor(Color: TQuadColor) : TColor;
  private
    FFont : TFont;
    FBrush : TBrush;
    FPen : TPen;
    FAttachedDC : HDC;

    function GetHandle() : HDC;

    procedure SetFont(NewFont : TFont);
    procedure SetBrush(NewBrush : TBrush);
    procedure SetPen(NewPen : TPen);

    function GetPenPos : TPoint;
    procedure SetPenPos(NewPos : TPoint);

    function GetWidth : Integer;
    function GetHeight : Integer;

    // Converts to non-premultiplied alpha
    function GetPixel(X, Y : Integer) : COLORREF;
    procedure SetPixel(X, Y: Integer; Color: Cardinal); overload;
    procedure SetPixel(X, Y: Integer; Color: Cardinal; Alpha: Byte); overload;

    // Direct pre-multiplied alpha
    function GetRawPixel(X, Y : Integer) : TQuadColor;
    procedure SetRawPixel(X, Y : Integer; Color : TQuadColor);

    procedure TextOutPreVista(const Rect : TRect; const Text: string; const Alignment : TAlignment; const Alpha : Byte);
    procedure TextOutVistaPlus(const ARect : TRect; const Text: string; const Alignment : TAlignment; const Alpha : Byte);
    function CanUseDrawThemeTextEx : boolean;
    procedure InternalGlowTextOut(const X, Y, GlowSize: Integer; const Text: string;
      const Alignment : TAlignment; const Alpha: Byte; const ProcessBackColor : Boolean;
      const BackColor : TQuadColor); overload;
    procedure InternalGlowTextOut(const ARect : TRect; const GlowSize: Integer; const Text: string;
      const Alignment : TAlignment; const Alpha: Byte; const ProcessBackColor : Boolean;
      const BackColor : TQuadColor); overload;
  protected
    FWorkingCanvas : TAlphaBitmapWrapper;

    function OrphanAliasedFont : HFONT;
  public
    constructor Create(Width, Height : Integer); overload;
    constructor Create(Canvas: TCanvas); overload;
    constructor Create(DC : HDC; Width, Height : Integer); overload;
    constructor Create(ToCopy : TCustomTransparentCanvas); overload;
    destructor Destroy; override;

    procedure Assign(Source : TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;

    procedure SaveToFile(const Filename : string);

    procedure Draw(const X, Y: Integer; Canvas: TCanvas; const Width, Height : Integer;
      const UseTransparentColor : Boolean = false; const TransparentColor : COLORREF = $0; const TransparentEdgeWidth :
      Integer = -1); overload;
    procedure Draw(const X, Y: Integer; const Metafile: TMetafile; const Width, Height : Integer; const Transparency : Byte = $FF); overload;
    procedure Draw(const X, Y : Integer; Other : TCustomTransparentCanvas; const Transparency : Byte = 255); overload;

    procedure DrawTo(const X, Y : Integer; Canvas : TCanvas; const TargetWidth, TargetHeight: Integer; const Transparency : Byte = $FF); overload;
    procedure DrawTo(const X, Y: Integer; DC: HDC; const TargetWidth, TargetHeight: Integer; const Transparency : Byte = $FF); overload;
    procedure DrawToGlass(const X, Y : Integer; DC : HDC; const Transparency : Byte = $FF);

    procedure Ellipse(const X1, Y1, X2, Y2: Integer; const Alpha : Byte = $FF); overload;
    procedure Ellipse(const Rect : TRect; const Alpha : Byte = $FF); overload;

    procedure MoveTo(const X, Y: Integer);

    procedure RoundRect(const X1, Y1, X2, Y2, XRadius, YRadius: Integer; const Alpha : Byte = $FF); overload;
    procedure RoundRect(const Rect : TRect; const XRadius, YRadius : Integer; const Alpha : Byte = $FF); overload;
    procedure Rectangle(const X1, Y1, X2, Y2: Integer; const Alpha : Byte = $FF); overload;
    procedure Rectangle(const Rect : TRect; const Alpha : Byte = $FF); overload;

    function TextExtent(const Text: string): TSize;
    function TextHeight(const Text: string): Integer;
    procedure TextOut(const X, Y: Integer; const Text: string; const Alignment : TAlignment = taLeftJustify; const Alpha : Byte = $FF);
    procedure TextRect(const Rect: TRect; const Text: string; const Alignment : TAlignment = taLeftJustify; const Alpha : Byte = $FF);
    function TextWidth(const Text: string): Integer;

    function CanDrawGlowText : boolean;
    procedure GlowTextOut(const X, Y, GlowSize: Integer; const Text: string;
      const Alignment : TAlignment = taLeftJustify; const Alpha : Byte = $FF);
    procedure GlowTextOutBackColor(const X, Y, GlowSize: Integer; const Text: string; const BackColor : TColor;
      const Alignment : TAlignment = taLeftJustify; const GlowAlpha : Byte = $FF; const Alpha : Byte = $FF);

    procedure Clear;

    property Handle: HDC read GetHandle;
    property PenPos: TPoint read GetPenPos write SetPenPos;
    property Pixels[X, Y: Integer]: COLORREF read GetPixel write SetPixel;
    property RawPixels[X, Y: Integer]: TQuadColor read GetRawPixel write SetRawPixel;
  published
    property Brush: TBrush read FBrush write SetBrush;
    property Font: TFont read FFont write SetFont;
    property Pen: TPen read FPen write SetPen;
    property Width : Integer read GetWidth;
    property Height : Integer read GetHeight;
  end;

  TTransparentCanvas = class(TCustomTransparentCanvas)
  end;

  TTransparentControlCanvas = class(TCustomTransparentCanvas)
  private
    FControl : TWinControl;
    FControlDC : HDC;
  public
    constructor Create(Control : TWinControl);
    destructor Destroy; override;
  end;

implementation

uses
  System.Math, Vcl.Themes, System.RTLConsts, Winapi.UxTheme;

{$if CompilerVersion >= 23.0} // XE2
  function InternalStyleServices : TCustomStyleServices;
  begin
    {$if declared(StyleServices)}
      Result := StyleServices;
    {$else}
      Result := ThemeServices; // Deprecated in favour of StyleServices
    {$ifend}
  end;
{$else}
  function InternalStyleServices : TThemeServices;
  begin
    Result := ThemeServices;
  end;
{$ifend}

function AlignmentToFlags(const Alignment : TAlignment) : DWORD;
begin
  Result := 0;
  case Alignment of
    taLeftJustify: Result := DT_LEFT;
    taRightJustify: Result := DT_RIGHT;
    taCenter: Result := DT_CENTER;
  end;
end;

{ TCustomTransparentCanvas }

function TCustomTransparentCanvas.CanDrawGlowText: boolean;
begin
  Result := CanUseDrawThemeTextEx;
end;

function TCustomTransparentCanvas.CanUseDrawThemeTextEx: boolean;
begin
  Result :=
    {$if declared(StyleServices)} // Can't test TCustomStyleServices.Enabled, assume deprecation follows StyleServices
      InternalStyleServices.Enabled
    {$else}
      InternalStyleServices.ThemesEnabled
    {$ifend}
    and (Win32MajorVersion >= 6);
end;

procedure TCustomTransparentCanvas.Clear;
begin
  FWorkingCanvas.Clear;
end;

constructor TCustomTransparentCanvas.Create(Width, Height : Integer);
begin
  inherited Create();
  FWorkingCanvas := TAlphaBitmapWrapper.CreateBlank(0, Width, Height);
  FFont := TFont.Create;
  FBrush := TBrush.Create;
  FPen := TPen.Create;
  FAttachedDC := 0;
end;

constructor TCustomTransparentCanvas.Create(ToCopy: TCustomTransparentCanvas);
begin
  inherited Create();
  FWorkingCanvas := TAlphaBitmapWrapper.Create(ToCopy.FWorkingCanvas);
  FFont := TFont.Create;
  FFont.Assign(ToCopy.FFont);
  FBrush := TBrush.Create;
  FBrush.Assign(ToCopy.FBrush);
  FPen := TPen.Create;
  FPen.Assign(ToCopy.FPen);
  FAttachedDC := 0;
end;

constructor TCustomTransparentCanvas.Create(Canvas: TCanvas);
begin
  inherited Create();
  FAttachedDC := Canvas.Handle;
  FWorkingCanvas := TAlphaBitmapWrapper.CreateBlank(Canvas.Handle, Width, Height);
  FFont := TFont.Create;
  FBrush := TBrush.Create;
  FPen := TPen.Create;
end;

constructor TCustomTransparentCanvas.Create(DC: HDC; Width, Height : Integer);
begin
  inherited Create();
  FAttachedDC := DC;
  FWorkingCanvas := TAlphaBitmapWrapper.CreateBlank(DC, Width, Height);
  FFont := TFont.Create;
  FBrush := TBrush.Create;
  FPen := TPen.Create;
end;

destructor TCustomTransparentCanvas.Destroy;
begin
  FreeAndNil(FWorkingCanvas);
  FreeAndNil(FFont);
  FreeAndNil(FBrush);
  FreeAndNil(FPen);

  inherited;
end;

procedure TCustomTransparentCanvas.Assign(Source : TPersistent);
var
  ToCopy : TCustomTransparentCanvas;
begin
  if Source is TCustomTransparentCanvas then begin
    ToCopy := Source as TCustomTransparentCanvas;

    FWorkingCanvas.Free;
    FWorkingCanvas := TAlphaBitmapWrapper.Create(ToCopy.FWorkingCanvas);
    assert(Assigned(FFont) and Assigned(FBrush) and Assigned(FPen));
    FFont.Assign(ToCopy.FFont);
    FBrush.Assign(ToCopy.FBrush);
    FPen.Assign(ToCopy.FPen);
    FAttachedDC := 0;
  end else begin
    if Assigned(Source) then
      raise EConvertError.CreateResFmt(@System.RTLConsts.SAssignError, [Source.ClassName, ClassName])
    else
      raise EConvertError.CreateResFmt(@System.RTLConsts.SAssignError, ['nil', ClassName]);
  end;
end;

procedure TCustomTransparentCanvas.AssignTo(Dest: TPersistent);
begin
  Dest.Assign(Self);
end;

procedure TCustomTransparentCanvas.SaveToFile(const Filename : string);
var
  BMP : TBitmap;
begin
  // Draw to a transparent 32-bit bitmap, and save that
  BMP := TBitmap.Create;
  try
    BMP.PixelFormat := pf32bit;
    BMP.Width := Width;
    BMP.Height := Height;
    DrawTo(0, 0, BMP.Canvas, Width, Height);
    BMP.SaveToFile(Filename);
  finally
    BMP.Free;
  end;
end;

procedure TCustomTransparentCanvas.Draw(const X, Y: Integer; Canvas: TCanvas; const Width, Height : Integer;
  const UseTransparentColor : Boolean; const TransparentColor : COLORREF; const TransparentEdgeWidth : Integer);
var
  TempImage : TAlphaBitmapWrapper;
begin
  TempImage := TAlphaBitmapWrapper.CreateForGDI(FWorkingCanvas.FDCHandle, Width, Height);
  try
    BitBlt(TempImage.FDCHandle, 0, 0, Width, Height, Canvas.Handle, 0, 0, SRCCOPY);
    TempImage.SetAllTransparency($FF); // No need to test, all written by GDI
    if UseTransparentColor then
      TempImage.ProcessTransparentColor(TransparentColor, TransparentEdgeWidth);
    TempImage.BlendTo(X, Y, FWorkingCanvas);
  finally
    TempImage.Free;
  end;
end;

procedure TCustomTransparentCanvas.Draw(const X, Y: Integer; const Metafile: TMetafile;
  const Width, Height: Integer; const Transparency : Byte = $FF);
var
  TempImage : TAlphaBitmapWrapper;
begin
  TempImage := TAlphaBitmapWrapper.CreateForGDI(FWorkingCanvas.FDCHandle, Width, Height);
  try
    TempImage.SelectObjects(TGDIObjects.CreateWithHandles(Brush.Handle, Pen.Handle, Font.Handle));
    try
      PlayEnhMetaFile(TempImage.FDCHandle, Metafile.Handle, Rect(0, 0, Width, Height));
      TempImage.ProcessTransparency(Transparency);
      TempImage.BlendTo(X, Y, FWorkingCanvas);
    finally
      TempImage.SelectOriginalObjects;
    end;
  finally
    TempImage.Free;
  end;
end;

procedure TCustomTransparentCanvas.Draw(const X, Y : Integer; Other : TCustomTransparentCanvas;
  const Transparency : Byte = 255);
begin
  Other.FWorkingCanvas.BlendTo(X, Y, FWorkingCanvas, Transparency);
end;

procedure TCustomTransparentCanvas.DrawTo(const X, Y: Integer; Canvas: TCanvas; const TargetWidth,
  TargetHeight: Integer; const Transparency : Byte = 255);
begin
  DrawTo(X, Y, Canvas.Handle, TargetWidth, TargetHeight, Transparency);
end;

procedure TCustomTransparentCanvas.DrawTo(const X, Y: Integer; DC: HDC; const TargetWidth,
  TargetHeight: Integer; const Transparency : Byte = 255);
var
  TempCanvas: TAlphaBitmapWrapper;
begin
  // Create a 32-bit canvas with a copy of the dc drawn in it with opaque alpha
  TempCanvas := TAlphaBitmapWrapper.CreateBlank(DC, TargetWidth, TargetHeight);
  try
    BitBlt(TempCanvas.FDCHandle, 0, 0, TargetWidth, TargetHeight, DC, 0, 0, SRCCOPY);
    TempCanvas.SetAllTransparency($FF);

    // Now blend the working image onto it at (X, Y), possibly stretched
    if (TargetWidth = Width) and (TargetHeight = Height) then begin
      FWorkingCanvas.BlendTo(X, Y, TempCanvas, Transparency);
    end else begin
      FWorkingCanvas.BlendToStretch(X, Y, TargetWidth, TargetHeight, TempCanvas, Transparency);
    end;

    // Now blit the composited image back to the DC
    BitBlt(DC, 0, 0, TargetWidth, TargetHeight, TempCanvas.FDCHandle, 0, 0, SRCCOPY);
  finally
    TempCanvas.Free;
  end;
end;

procedure TCustomTransparentCanvas.DrawToGlass(const X, Y: Integer; DC: HDC; const Transparency : Byte);
begin
  FWorkingCanvas.BlendToDC(X, Y, DC, Transparency);
end;

procedure TCustomTransparentCanvas.Ellipse(const X1, Y1, X2, Y2: Integer; const Alpha: Byte);
var
  TempImage : TAlphaBitmapWrapper;
begin
  TempImage := TAlphaBitmapWrapper.CreateForGDI(FWorkingCanvas.FDCHandle, X2-X1, Y2-Y1);
  try
    TempImage.SelectObjects(TGDIObjects.CreateWithHandles(Brush.Handle, Pen.Handle, Font.Handle));
    SetWindowOrgEx(TempImage.FDCHandle, X1 - Pen.Width div 2, Y1 - Pen.Width div 2, nil);
    Winapi.Windows.Ellipse(TempImage.FDCHandle, X1, Y1, X2, Y2);
    SetWindowOrgEx(TempImage.FDCHandle, 0, 0, nil);
    TempImage.ProcessTransparency(Alpha);
    TempImage.BlendTo(X1, Y1, FWorkingCanvas);
    TempImage.SelectOriginalObjects;
  finally
    TempImage.Free;
  end;
end;

procedure TCustomTransparentCanvas.Ellipse(const Rect: TRect; const Alpha: Byte);
begin
  Ellipse(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, Alpha);
end;

function TCustomTransparentCanvas.GetHandle: HDC;
begin
  Result := FWorkingCanvas.FDCHandle;
end;

function TCustomTransparentCanvas.GetHeight: Integer;
begin
  Result := FWorkingCanvas.FHeight;
end;

function TCustomTransparentCanvas.GetPenPos: TPoint;
begin
  GetCurrentPositionEx(FWorkingCanvas.FDCHandle, @Result);
end;

function TCustomTransparentCanvas.GetPixel(X, Y: Integer): COLORREF;
begin
  Result := GetRawPixel(X, Y).AsColorRef;
end;

function TCustomTransparentCanvas.GetRawPixel(X, Y: Integer): TQuadColor;
var
  PQuad : PQuadColor;
  InverseY : Integer;
begin
  InverseY := FWorkingCanvas.FHeight - Y - 1; // 0 is top not bottom
  PQuad := FWorkingCanvas.FQuads;
  Inc(PQuad, (InverseY * Width) + X);
  Result := PQuad^;
end;

function TCustomTransparentCanvas.GetWidth: Integer;
begin
  Result := FWorkingCanvas.FWidth;
end;

procedure TCustomTransparentCanvas.InternalGlowTextOut(const X, Y, GlowSize: Integer; const Text: string;
  const Alignment : TAlignment; const Alpha: Byte; const ProcessBackColor : Boolean; const BackColor : TQuadColor);
var
  TextSize : TSize;
begin
  TextSize := TextExtent(Text);
  InternalGlowTextOut(Rect(X, Y, X + TextSize.cx, Y + TextSize.cy), GlowSize, Text, Alignment, Alpha, ProcessBackColor, BackColor);
end;

procedure TCustomTransparentCanvas.InternalGlowTextOut(const ARect : TRect; const GlowSize: Integer; const Text: string;
  const Alignment : TAlignment; const Alpha: Byte; const ProcessBackColor : Boolean;
  const BackColor : TQuadColor);
var
  TempImage : TAlphaBitmapWrapper;
  TextSize : TSize;
  Options : TDTTOpts;
  Details: TThemedElementDetails;
  TextRect : TRect;
  AlignFlags : DWORD;
begin
  if Length(Text) = 0 then Exit; // Crash creating zero-sized bitmap
  if not CanDrawGlowText then raise ETransparentCanvasException.Create('Cannot use DrawThemeTextEx');

  TextSize := TextExtent(Text);
  AlignFlags := AlignmentToFlags(Alignment);
  TempImage := TAlphaBitmapWrapper.CreateForDrawThemeTextEx(FWorkingCanvas.FDCHandle, TextSize.cx + GlowSize*2, TextSize.cy + GlowSize*2);
  try
    TempImage.SelectObjects(TGDIObjects.CreateWithHandles(0, 0, Font.Handle));
    SetBkMode(TempImage.FDCHandle, TRANSPARENT);
    SetTextColor(TempImage.FDCHandle, ColorToRGB(Font.Color));

		ZeroMemory(@Options, SizeOf(Options));
		Options.dwSize := SizeOf(Options);
		Options.dwFlags := DTT_TEXTCOLOR or DTT_COMPOSITED or DTT_GLOWSIZE;
		Options.crText := ColorToRGB(Font.Color);
		Options.iGlowSize := GlowSize;

		Details := InternalStyleServices.GetElementDetails(teEditTextNormal);
    TextRect := Rect(GlowSize, GlowSize, TextSize.cx + GlowSize*2, TextSize.cy + GlowSize*2);
    DrawThemeTextEx(InternalStyleServices.Theme[teEdit], TempImage.FDCHandle, Details.Part, Details.State,
      PChar(Text), Length(Text), AlignFlags or DT_TOP or DT_NOCLIP, TextRect,
      Options);

    if ProcessBackColor then begin
      TempImage.TintByAlphaToColor(BackColor);
      // Now draw the text over again, but with no glow, so only the text is drawn
      TextRect := Rect(GlowSize, GlowSize, TextSize.cx + GlowSize, TextSize.cy + GlowSize);
      Options.dwFlags := DTT_TEXTCOLOR or DTT_COMPOSITED;
      Options.crText := ColorToRGB(Font.Color);
      Options.iGlowSize := 0;
      DrawThemeTextEx(InternalStyleServices.Theme[teEdit], TempImage.FDCHandle, Details.Part, Details.State,
        PChar(Text), Length(Text), AlignFlags or DT_TOP or DT_NOCLIP, TextRect,
        Options);
    end;

    case Alignment of
      taLeftJustify: TempImage.BlendTo(ARect.Left - GlowSize, ARect.Top - GlowSize, FWorkingCanvas, Alpha);
      taRightJustify: TempImage.BlendTo(ARect.Left - GlowSize * 2, ARect.Top - GlowSize, FWorkingCanvas, Alpha);
      taCenter: TempImage.BlendTo(ARect.Left - GlowSize * 2 + 1 + IfThen(GlowSize > 0, 1, -1), ARect.Top - GlowSize, FWorkingCanvas, Alpha);
    end;
    SetBkMode(TempImage.FDCHandle, OPAQUE);
    TempImage.SelectOriginalObjects;
  finally
    TempImage.Free;
  end;
end;

procedure TCustomTransparentCanvas.GlowTextOut(const X, Y, GlowSize: Integer; const Text: string;
  const Alignment : TAlignment; const Alpha: Byte);
begin
  InternalGlowTextOut(X, Y, GlowSize, Text, Alignment, Alpha, False, TQuadColor.Create(0));
end;

procedure TCustomTransparentCanvas.GlowTextOutBackColor(const X, Y, GlowSize: Integer;
  const Text: string; const BackColor: TColor; const Alignment : TAlignment;
  const GlowAlpha : Byte; const Alpha: Byte);
var
  Background : TQuadColor;
begin
  if (COLORREF(ColorToRGB(BackColor)) = RGB(255, 255, 255)) and (GlowAlpha = 255) then begin // White is the default on
  //Windows; do no special processing
    GlowTextOut(X, Y, GlowSize, Text, Alignment, Alpha);
  end else begin
    // Windows draws glowing text with a white background, always.  To change the background colour,
    // draw with the normal white background and black text, then process the colours to change
    // white to the specified colour, and black to the font colour
    Background := TQuadColor.Create(BackColor);
    Background.SetAlpha(GlowAlpha, Alpha / 255.0);
    InternalGlowTextOut(X, Y, GlowSize, Text, Alignment, Alpha, True, Background);
  end;
end;

procedure TCustomTransparentCanvas.MoveTo(const X, Y: Integer);
begin
  MoveToEx(FWorkingCanvas.FDCHandle, X, Y, nil);
end;

function TCustomTransparentCanvas.OrphanAliasedFont: HFONT;
var
  FontWeight : Cardinal;
begin
  // Font output and alpha is tricky with a ClearType or antialiased font.  This method takes FFont
  // and creates a new font with the same attributes, but with ClearType and AA explicitly disabled
  if fsBold in Font.Style then
    FontWeight := FW_BOLD
  else
    FontWeight := FW_NORMAL;
  Result := CreateFont(FFont.Height, 0, 0, 0, FontWeight, Cardinal(fsItalic in Font.Style),
    Cardinal(fsUnderline in Font.Style), Cardinal(fsStrikeOut in Font.Style), DEFAULT_CHARSET,
    OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, ANTIALIASED_QUALITY, DEFAULT_PITCH, PChar(Font.Name));
end;

class function TCustomTransparentCanvas.QuadColorToTColor(Color: TQuadColor): TColor;
begin
  Result := TColor(RGB(Color.Red, Color.Blue, Color.Green));
end;

procedure TCustomTransparentCanvas.Rectangle(const X1, Y1, X2, Y2: Integer; const Alpha: Byte);
var
  TempImage : TAlphaBitmapWrapper;
begin
  TempImage := TAlphaBitmapWrapper.CreateForGDI(FWorkingCanvas.FDCHandle, X2-X1, Y2-Y1);
  try
    TempImage.SelectObjects(TGDIObjects.CreateWithHandles(Brush.Handle, Pen.Handle, Font.Handle));
    SetWindowOrgEx(TempImage.FDCHandle, X1 - Pen.Width div 2, Y1 - Pen.Width div 2, nil);
    Winapi.Windows.Rectangle(TempImage.FDCHandle, X1, Y1, X2, Y2);
    SetWindowOrgEx(TempImage.FDCHandle, 0, 0, nil);
    TempImage.ProcessTransparency(Alpha);
    TempImage.BlendTo(X1, Y1, FWorkingCanvas);
    TempImage.SelectOriginalObjects;
  finally
    TempImage.Free;
  end;
end;

procedure TCustomTransparentCanvas.Rectangle(const Rect: TRect; const Alpha: Byte);
begin
  Rectangle(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, Alpha);
end;

procedure TCustomTransparentCanvas.RoundRect(const Rect: TRect; const XRadius, YRadius : Integer; const Alpha : Byte = $FF);
begin
  RoundRect(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, XRadius, YRadius, Alpha);
end;

procedure TCustomTransparentCanvas.RoundRect(const X1, Y1, X2, Y2, XRadius, YRadius: Integer; const Alpha : Byte = $FF);
var
  TempImage : TAlphaBitmapWrapper;
begin
  TempImage := TAlphaBitmapWrapper.CreateForGDI(FWorkingCanvas.FDCHandle, X2-X1 + Pen.Width, Y2-Y1 + Pen.Width);
  try
    TempImage.SelectObjects(TGDIObjects.CreateWithHandles(Brush.Handle, Pen.Handle, Font.Handle));
    SetWindowOrgEx(TempImage.FDCHandle, X1 - Pen.Width div 2, Y1 - Pen.Width div 2, nil);
    Winapi.Windows.RoundRect(TempImage.FDCHandle, X1, Y1, X2, Y2, XRadius, YRadius);
    SetWindowOrgEx(TempImage.FDCHandle, 0, 0, nil);
    TempImage.ProcessTransparency(Alpha);
    TempImage.BlendTo(X1, Y1, FWorkingCanvas);
    TempImage.SelectOriginalObjects;
  finally
    TempImage.Free;
  end;
end;

procedure TCustomTransparentCanvas.SetBrush(NewBrush: TBrush);
begin
  FBrush.Assign(NewBrush);
end;

procedure TCustomTransparentCanvas.SetFont(NewFont: TFont);
begin
  FFont.Assign(NewFont);
end;

procedure TCustomTransparentCanvas.SetPen(NewPen: TPen);
begin
  FPen.Assign(NewPen);
end;

procedure TCustomTransparentCanvas.SetPenPos(NewPos: TPoint);
begin
  MoveToEx(FWorkingCanvas.FDCHandle, NewPos.X, NewPos.Y, nil);
end;

procedure TCustomTransparentCanvas.SetPixel(X, Y: Integer; Color: Cardinal);
begin
  SetPixel(X, Y, Color, $FF);
end;

procedure TCustomTransparentCanvas.SetPixel(X, Y: Integer; Color: Cardinal; Alpha: Byte);
var
  PQuad : PQuadColor;
  InverseY : Integer;
begin
  InverseY := FWorkingCanvas.FHeight - Y - 1; // 0 is top not bottom
  PQuad := FWorkingCanvas.FQuads;
  Inc(PQuad, (InverseY * Width) + X);
  PQuad.Quad := Color;
  PQuad.Alpha := Alpha;
end;

procedure TCustomTransparentCanvas.SetRawPixel(X, Y: Integer; Color: TQuadColor);
var
  PQuad : PQuadColor;
  InverseY : Integer;
begin
  InverseY := FWorkingCanvas.FHeight - Y - 1; // 0 is top not bottom
  PQuad := FWorkingCanvas.FQuads;
  Inc(PQuad, (InverseY * Width) + X);
  PQuad.Quad := Color.Quad;
end;

class function TCustomTransparentCanvas.TColorToQuadColor(Color: TColor): TQuadColor;
begin
  Result := TQuadColor.Create(Color);
end;

function TCustomTransparentCanvas.TextExtent(const Text: string): TSize;
var
  OldFontHandle,
  FontHandle : HFONT;
begin
  if Length(Text) = 0 then begin
    Result.cx := 0;
    Result.cy := 0;
    Exit;
  end;

  if CanUseDrawThemeTextEx then begin // Can use DrawThemeTextEx; just get text extent normally
    FWorkingCanvas.SelectObjects(TGDIObjects.CreateWithHandles(0, 0, Font.Handle));
    GetTextExtentPoint32(FWorkingCanvas.FDCHandle, PChar(Text), Length(Text), Result);
    FWorkingCanvas.SelectOriginalObjects;
  end else begin
    // Can't use DrawThemeTextEx; use aliased font (may affect output size, so need to explicitly
    // measure using the aliased font)
    FontHandle := OrphanAliasedFont;
    try
      OldFontHandle := SelectObject(FWorkingCanvas.FDCHandle, FontHandle);
      GetTextExtentPoint32(FWorkingCanvas.FDCHandle, PChar(Text), Length(Text), Result);
      SelectObject(FWorkingCanvas.FDCHandle, OldFontHandle);
    finally
      DeleteObject(FontHandle);
    end;
  end;
end;

function TCustomTransparentCanvas.TextHeight(const Text: string): Integer;
begin
  Result := TextExtent(Text).cy;
end;

procedure TCustomTransparentCanvas.TextOut(const X, Y: Integer; const Text: string;
  const Alignment : TAlignment; const Alpha : Byte);
var
  TextSize : TSize;
begin
  if Length(Text) = 0 then Exit; // Crash creating zero-sized bitmap

  TextSize := TextExtent(Text);
  if CanUseDrawThemeTextEx then
    TextOutVistaPlus(Rect(X, Y, X + TextSize.cx, Y + TextSize.cy), Text, Alignment, Alpha)
  else
    TextOutPreVista(Rect(X, Y, X + TextSize.cx, Y + TextSize.cy), Text, Alignment, Alpha);
end;

procedure TCustomTransparentCanvas.TextOutPreVista(const Rect: TRect; const Text: string;
  const Alignment : TAlignment; const Alpha: Byte);
var
  TempImage : TAlphaBitmapWrapper;
  FontHandle : HFONT;
  TextSize : TSize;
  OldAlign : UINT;
begin
  if Length(Text) = 0 then Exit; // Crash creating zero-sized bitmap

  TextSize := TextExtent(Text);
  // Clip to the rest by restricting the size it thinks the text is - the bitmap will be this size, thus clipped
  TextSize.cx := min(TextSize.cx, Rect.Right-Rect.Left);
  TextSize.cy := min(TextSize.cy, Rect.Bottom-Rect.Top);
  FontHandle := OrphanAliasedFont; // Antialiased or cleartype text works terribly when trying to fix the alpha
  TempImage := TAlphaBitmapWrapper.CreateForGDI(FWorkingCanvas.FDCHandle, TextSize.cx, TextSize.cy);
  try
    TempImage.SelectObjects(TGDIObjects.CreateWithHandles(0, 0, FontHandle));
    SetBkMode(TempImage.FDCHandle, TRANSPARENT);
    SetTextColor(TempImage.FDCHandle, ColorToRGB(Font.Color));
    OldAlign := GetTextAlign(TempImage.FDCHandle);
    try
      case Alignment of
        taLeftJustify: SetTextAlign(TempImage.FDCHandle, TA_LEFT);
        taRightJustify: SetTextAlign(TempImage.FDCHandle, TA_RIGHT);
        taCenter: SetTextAlign(TempImage.FDCHandle, TA_CENTER);
      end;
      ExtTextOut(TempImage.FDCHandle, 0, 0, ETO_CLIPPED, nil, PChar(Text), Length(Text), nil);
    finally
      SetTextAlign(TempImage.FDCHandle, OldAlign);
    end;
    SetBkMode(TempImage.FDCHandle, OPAQUE);
    TempImage.ProcessTransparency(Alpha);
    TempImage.BlendTo(Rect.Left, Rect.Top, FWorkingCanvas);
    TempImage.SelectOriginalObjects;
  finally
    DeleteObject(FontHandle);
    TempImage.Free;
  end;
end;

procedure TCustomTransparentCanvas.TextOutVistaPlus(const ARect: TRect; const Text: string;
  const Alignment : TAlignment; const Alpha: Byte);
var
  TempImage : TAlphaBitmapWrapper;
  TextSize : TSize;
  Options : TDTTOpts;
  Details: TThemedElementDetails;
  TextRect : TRect;
  AlignFlags : DWORD;
begin
  if Length(Text) = 0 then Exit; // Crash creating zero-sized bitmap
  if not CanUseDrawThemeTextEx then raise ETransparentCanvasException.Create('Cannot use DrawThemeTextEx');

  AlignFlags := AlignmentToFlags(Alignment);
  TextSize := TextExtent(Text);
  // Clip by clipping the size of the rectangle it assumes the text fits in
  TextSize.cx := min(TextSize.cx, ARect.Right-ARect.Left);
  TextSize.cy := min(TextSize.cy, ARect.Bottom-ARect.Top);
  TempImage := TAlphaBitmapWrapper.CreateForDrawThemeTextEx(FWorkingCanvas.FDCHandle, TextSize.cx, TextSize.cy);
  try
    TempImage.SelectObjects(TGDIObjects.CreateWithHandles(0, 0, Font.Handle));
    SetBkMode(TempImage.FDCHandle, TRANSPARENT);
    SetTextColor(TempImage.FDCHandle, ColorToRGB(Font.Color));

		ZeroMemory(@Options, SizeOf(Options));
		Options.dwSize := SizeOf(Options);
		Options.dwFlags := DTT_TEXTCOLOR or DTT_COMPOSITED;
		Options.crText := ColorToRGB(Font.Color);
		Options.iGlowSize := 0;

		Details := InternalStyleServices.GetElementDetails(teEditTextNormal);
    TextRect := Rect(0, 0, TextSize.cx, TextSize.cy);
    DrawThemeTextEx(InternalStyleServices.Theme[teEdit], TempImage.FDCHandle, Details.Part, Details.State,
      PChar(Text), Length(Text), AlignFlags or DT_TOP, TextRect,
      Options);

    SetBkMode(TempImage.FDCHandle, OPAQUE);
    TempImage.BlendTo(ARect.Left, ARect.Top, FWorkingCanvas, Alpha);
    TempImage.SelectOriginalObjects;
  finally
    TempImage.Free;
  end;
end;

procedure TCustomTransparentCanvas.TextRect(const Rect: TRect; const Text: string; const Alignment : TAlignment; const Alpha: Byte);
begin
  if Length(Text) = 0 then Exit; // Crash creating zero-sized bitmap

  if CanUseDrawThemeTextEx then
    TextOutVistaPlus(Rect, Text, Alignment, Alpha)
  else
    TextOutPreVista(Rect, Text, Alignment, Alpha);
end;

function TCustomTransparentCanvas.TextWidth(const Text: string): Integer;
begin
  Result := TextExtent(Text).cx;
end;

{ TAlphaBitmapWrapper }

procedure TAlphaBitmapWrapper.BlendTo(X, Y: Integer; var Image: TAlphaBitmapWrapper; Transparency: Byte);
var
  BlendFunc : TBlendFunction;
begin
  with BlendFunc do begin
    BlendOp := AC_SRC_OVER;
    BlendFlags := 0;
    SourceConstantAlpha := Transparency; // Normally 255
    AlphaFormat := AC_SRC_ALPHA;
  end;
  AlphaBlend(Image.FDCHandle, X, Y, FWidth, FHeight, FDcHandle, 0, 0, FWidth, FHeight, BlendFunc);
end;

procedure TAlphaBitmapWrapper.BlendToDC(X, Y: Integer; DC: HDC; Transparency: Byte);
var
  BlendFunc : TBlendFunction;
begin
  with BlendFunc do begin
    BlendOp := AC_SRC_OVER;
    BlendFlags := 0;
    SourceConstantAlpha := Transparency;
    AlphaFormat := AC_SRC_ALPHA;
  end;
  AlphaBlend(DC, X, Y, FWidth, FHeight, FDcHandle, 0, 0, FWidth, FHeight, BlendFunc);
end;

procedure TAlphaBitmapWrapper.BlendToStretch(X, Y, StretchWidth, StretchHeight: Integer; var Image: TAlphaBitmapWrapper; Transparency: Byte);
var
  BlendFunc : TBlendFunction;
begin
  with BlendFunc do begin
    BlendOp := AC_SRC_OVER;
    BlendFlags := 0;
    SourceConstantAlpha := Transparency; // Normally 255
    AlphaFormat := AC_SRC_ALPHA;
  end;
  AlphaBlend(Image.FDCHandle, 0, 0,StretchWidth, StretchHeight, FDcHandle, X, Y, FWidth, FHeight, BlendFunc);
end;

procedure TAlphaBitmapWrapper.CheckHandles;
begin
  if FDCHandle = 0 then
    raise ETransparentCanvasException.Create('Cannot create device context');
  if FBMPHandle = 0 then
    raise ETransparentCanvasException.Create('Cannot create 32-bit bitmap');
  if FQuads = nil then
    raise ETransparentCanvasException.Create('Cannot access bitmap bits');
end;

procedure TAlphaBitmapWrapper.Clear;
begin
  ZeroMemory(FQuads, FWidth * FHeight * SizeOf(TQuadColor));
end;

procedure TAlphaBitmapWrapper.Construct(DC: HDC; Empty: Boolean; Width, Height: Integer; Inverted : boolean);
var
  BMPInfo : TBitmapInfo;
  PQuads : Pointer;
begin
  FWidth := Width;
  FHeight := Height;
  if (FWidth <= 0) or (FHeight <= 0) then
    raise ETransparentCanvasException.Create('Invalid size specified; Width and Height must both be greater than zero.');
  FDCHandle := CreateCompatibleDC(DC);
  ZeroMemory(@BMPInfo, SizeOf(TBitmapInfo));
  with BMPInfo.bmiHeader do begin
    biSize := SizeOf(TBitmapInfo);
    biWidth := FWidth;
    if Inverted then begin
      biHeight := -FHeight // For DrawThemeTextEx: requires inverted (bottom-up) bitmap
    end else begin
      biHeight := FHeight;
    end;
    biPlanes := 1;
    biBitCount := 32;
    biCompression := BI_RGB;
    biSizeImage := FWidth * FHeight * SizeOf(TQuadColor);
  end;
  PQuads := nil;
  FBMPHandle := 0;
  FBMPHandle := CreateDIBSection(FDCHandle, BMPInfo, DIB_RGB_COLORS, PQuads, 0, 0);
  FQuads := PQuadColor(PQuads);
  CheckHandles;
  FOriginalBMP := SelectObject(FDCHandle, FBMPHandle);
  GdiFlush; // Need to flush before any manipulation of bits
  if Empty then begin
    ZeroMemory(FQuads, Width * Height * SizeOf(TQuadColor));
  end else begin
    FillMemory(FQuads, Width * Height * SizeOf(TQuadColor), $FF);
  end;
end;

constructor TAlphaBitmapWrapper.Create(var ToCopy: TAlphaBitmapWrapper);
begin
  inherited Create();
  Construct(ToCopy.FDCHandle, true, ToCopy.FWidth, ToCopy.FHeight); // true = init to all zeroes
  ToCopy.BlendTo(0, 0, Self);
end;

constructor TAlphaBitmapWrapper.CreateBlank(DC: HDC; Width, Height: Integer);
begin
  inherited Create();
  Construct(DC, true, Width, Height); // true = init to all zeroes
end;

constructor TAlphaBitmapWrapper.CreateForDrawThemeTextEx(DC: HDC; Width, Height: Integer; DummyDrawThemeTextEx : SmallInt = 0);
begin
  inherited Create();
  Construct(DC, true, Width, Height, true); // init to all zeroes; inverted (upside down) because DrawThemeTextEx needs it
end;

constructor TAlphaBitmapWrapper.CreateForGDI(DC: HDC; Width, Height: Integer; DummyGDI : Byte = 0);
begin
  inherited Create();
  Construct(DC, false, Width, Height); // false = init all bytes to $FF, so can test if written to
end;

destructor TAlphaBitmapWrapper.Destroy;
begin
  SelectOriginalObjects;
  SelectObject(FDCHandle, FOriginalBMP);
  DeleteObject(FBMPHandle);
  FBMPHandle := 0;
  DeleteObject(FDCHandle);
  FDCHandle := 0;
  inherited;
end;

procedure TAlphaBitmapWrapper.TintByAlphaToColor(const Color: TQuadColor);
var
  Loop : Integer;
  PQuad : PQuadColor;
begin
  // Change the background colour of glowing text by changing white to BackColor, and black to
  // TextColor.  Alpha remains the same
  GdiFlush; // Need to flush before any manipulation of bits
  PQuad := FQuads;
  for Loop := 0 to FWidth * FHeight - 1 do begin
    if PQuad.Alpha <> 0 then begin
      PQuad.SetFromColorMultAlpha(Color); // Sets the colour, and multiplies the alphas together
    end;
    Inc(PQuad);
  end;
end;

procedure TAlphaBitmapWrapper.ProcessMaskTransparency(var MaskImage: TAlphaBitmapWrapper);
var
  Loop : Integer;
  PQuad,
  PMaskQuad : PQuadColor;
begin
  if not ((FWidth = MaskImage.FWidth)) and (FHeight = MaskImage.FHeight) then
    raise ETransparentCanvasException.Create('Mask images must be identical in size');

  GdiFlush; // Need to flush before any manipulation of bits
  PQuad := FQuads;
  PMaskQuad := MaskImage.FQuads;
  for Loop := 0 to FWidth * FHeight - 1 do begin
    if (PMaskQuad.Quad and $00FFFFFF) = 0 then begin
      PQuad.SetAlpha(255, 1.0);
    end else begin
      PQuad.Quad := 0;
    end;
    Inc(PQuad);
    Inc(PmaskQuad);
  end;
end;

procedure TAlphaBitmapWrapper.ProcessTransparentColor(const TransparentColor : COLORREF; const TransparentEdgeWidth : Integer);
  function IsEdge(const PixelIndex : Integer) : Boolean;
  var
    X, Y : Integer;
  begin
    if TransparentEdgeWidth < 0 then Exit(true); // Entire image should be processed
    // index = (Y * width) + X (note Y is inverse)
    Y := PixelIndex div FWidth;
    X := PixelIndex - (Y * FWidth);
    Result := (X < TransparentEdgeWidth) or (Y < TransparentEdgeWidth) or
      (X > (FWidth - TransparentEdgeWidth - 1)) or (Y > (FHeight - TransparentEdgeWidth - 1));
  end;
var
  Loop : Integer;
  PQuad : PQuadColor;
  R, G, B : Byte;
begin
  if TransparentEdgeWidth = 0 then Exit; // Want to process an edge, but no edge width (pass -1 for whole image)

  GdiFlush; // Need to flush before any manipulation of bits
  R := GetRValue(TransparentColor);
  G := GetGValue(TransparentColor);
  B := GetBValue(TransparentColor);
  PQuad := FQuads;
  for Loop := 0 to FWidth * FHeight - 1 do begin
    if (PQuad.Red = R) and (PQuad.Green = G) and (PQuad.Blue = B) and IsEdge(Loop) then begin
      PQuad.SetAlpha(0, 0); // 32-bit OSes must have all channels 0 (premultiplied) for 0 alpha
    end;
    Inc(PQuad);
  end;
end;

procedure TAlphaBitmapWrapper.ProcessTransparency(const Alpha: Byte; TranspRect: TRect);
var
  LoopX : Integer;
  PreMult : Single;
  PQuad : PQuadColor;
  LoopY: Integer;
begin
  GdiFlush; // Need to flush before any manipulation of bits
  IntersectRect(TranspRect, TranspRect, Rect(0, 0, FWidth, FHeight)); // Clip to valid bounds

  PreMult := Alpha / 255.0;
  for LoopY := TranspRect.Top to TranspRect.Bottom - 1 do begin
    PQuad := FQuads;
    Inc(PQuad, LoopY);
    for LoopX := TranspRect.Left to TranspRect.Right - 1 do begin
      if PQuad.WrittenByGDI then begin
        PQuad.SetAlpha(Alpha, PreMult);
      end else begin
        PQuad.Quad := 0;
      end;
      Inc(PQuad);
    end;
  end;
end;

procedure TAlphaBitmapWrapper.ProcessTransparency(const Alpha: Byte);
var
  Loop : Integer;
  PreMult : Single;
  PQuad : PQuadColor;
begin
  GdiFlush; // Need to flush before any manipulation of bits
  PreMult := Alpha / 255.0;
  PQuad := FQuads;
  for Loop := 0 to FWidth * FHeight - 1 do begin
    if PQuad.WrittenByGDI then begin
      PQuad.SetAlpha(Alpha, PreMult);
    end else begin
      PQuad.Quad := 0;
    end;
    Inc(PQuad);
  end;
end;

function TAlphaBitmapWrapper.QuadPointer: PQuadColor;
begin
  Result := FQuads;
end;

function TAlphaBitmapWrapper.GetRawPixelPtr(const X, Y: Integer): PQuadColor;
var
  PQuad : PQuadColor;
  InverseY : Integer;
begin
  if (X >= 0) and (X < FWidth) and (Y >= 0) and (Y < FHeight) then begin
    InverseY := FHeight - Y - 1; // 0 is top not bottom
    PQuad := FQuads;
    Inc(PQuad, (InverseY * FWidth) + X);
    Result := PQuad;
  end else begin
    Result := nil;
  end;
end;

procedure TAlphaBitmapWrapper.SafeSetRawPixel(const X, Y: Integer; Color: TQuadColor);
var
  PQuad : PQuadColor;
  InverseY : Integer;
begin
  if (X >= 0) and (X < FWidth) and (Y >= 0) and (Y < FHeight) then begin
    InverseY := FHeight - Y - 1; // 0 is top not bottom
    PQuad := FQuads;
    Inc(PQuad, (InverseY * FWidth) + X);
    PQuad.Quad := Color.Quad;
  end;
end;

procedure TAlphaBitmapWrapper.SelectObjects(const GDI: TGDIObjects);
begin
  // This is only one layer deep - it stores the old objects in FOriginalObjects
  // If you call if more than once, it will overwrite the values in FOriginalObjects
  // If you find yourself doing this, this needs to be rewritten as a stack, and you'd
  // push and pop the handles.
  if (FOriginalObjects.FBrush <> 0) or (FOriginalObjects.FPen <> 0) or (FOriginalObjects.FFont <> 0) then
    raise ETransparentCanvasException.Create('SelectObjects has already been called');

  FOriginalObjects.FBrush := SelectObject(FDCHandle, GDI.Brush);
  FOriginalObjects.FPen := SelectObject(FDCHandle, GDI.Pen);
  FOriginalObjects.FFont := SelectObject(FDCHandle, GDI.Font);
end;

procedure TAlphaBitmapWrapper.SelectOriginalObjects;
begin
  SelectObject(FDCHandle, FOriginalObjects.FBrush);
  FOriginalObjects.FBrush := 0;
  SelectObject(FDCHandle, FOriginalObjects.FPen);
  FOriginalObjects.FPen := 0;
  SelectObject(FDCHandle, FOriginalObjects.FFont);
  FOriginalObjects.FFont := 0;
end;

procedure TAlphaBitmapWrapper.SetAllTransparency(const Alpha: Byte);
var
  Loop : Integer;
  PreMult : Single;
  PQuad : PQuadColor;
begin
  GdiFlush; // Need to flush before any manipulation of bits
  PreMult := Alpha / 255.0;
  PQuad := FQuads;
  for Loop := 0 to FWidth * FHeight - 1 do begin
    PQuad.SetAlpha(Alpha, PreMult);
    Inc(PQuad);
  end;
end;

{ TQuadColor }

function TQuadColor.AsColorRef: COLORREF;
var
  PreDiv : Single;
begin
  // contains premultiplied alpha, so un-premultiply it and return that
  if Alpha = 0 then begin
    Result := $00000000;
  end else begin
    PreDiv := 1 / (Alpha / 255.0);
    Result := RGB(Trunc(Red * PreDiv), Trunc(Green * PreDiv), Trunc(Blue * PreDiv)) or (Alpha shl 24);
  end;
end;

procedure TQuadColor.Clear;
begin
  Quad := 0;
end;

constructor TQuadColor.Create(Color: TColor);
var
  ColorRGB : COLORREF;
begin
  Alpha := 255;
  ColorRGB := ColorToRGB(Color);
  Red := GetRValue(ColorRGB);
  Green := GetGValue(ColorRGB);
  Blue := GetBValue(ColorRGB);
end;

procedure TQuadColor.SetAlpha(const Transparency: Byte; const PreMult: Single);
begin
  Alpha := Transparency;
  Blue := Trunc(Blue * PreMult);
  Green := Trunc(Green * PreMult);
  Red := Trunc(Red * PreMult);
end;

procedure TQuadColor.SetFromColorRef(const Color: COLORREF);
begin
  // Sets the colour, but keeps the current alpha
  if Alpha = 0 then begin
    Quad := 0;
  end else begin
    Red := GetRValue(Color);
    Green := GetGValue(Color);
    Blue := GetBValue(Color);
    SetAlpha(Alpha, Alpha / 255.0);
  end;
end;

procedure TQuadColor.SetFromColorMultAlpha(const Color: TQuadColor);
var
  MultAlpha : Byte;
begin
  Red := Color.Red;
  Green := Color.Green;
  Blue := Color.Blue;
  MultAlpha := Round(Integer(Alpha) * Integer(Color.Alpha) / 255.0);
  SetAlpha(MultAlpha, MultAlpha / 255.0);
end;

function TQuadColor.WrittenByGDI: Boolean;
begin
  Result := (Alpha = 0);
end;

{ TTransparentControlCanvas }

constructor TTransparentControlCanvas.Create(Control: TWinControl);
begin
  if Control = nil then
    raise ETransparentCanvasException.Create('Control must not be nil');
  if Control.Handle = 0 then
    raise ETransparentCanvasException.Create('Cannot access control handle');

  FControl := Control;
  FControlDC := GetWindowDC(Control.Handle);
  if FControlDC = 0 then
    raise ETransparentCanvasException.Create('Cannot obtain control device context');

  inherited Create(FControlDC, Control.Width, Control.Height);
end;

destructor TTransparentControlCanvas.Destroy;
begin
  DrawTo(0, 0, FControlDC, FControl.Width, FControl.Height);

  ReleaseDC(FControl.Handle, FControlDC);
  FControlDC := 0;
  FWorkingCanvas.FDCHandle := 0;

  inherited;
end;

{ TGDIObjects }

constructor TGDIObjects.CreateWithHandles(const hBrush: HBRUSH; const hPen: HPEN; const hFont : HFONT);
begin
  FBrush := hBrush;
  FPen := hPen;
  FFont := hFont;
end;

end.
