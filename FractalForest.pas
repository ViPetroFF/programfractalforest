{ Sample Program # 12, Source: FractalForest.pas, Date: 04.05.2011 0:14:38

  Copyright (C) 2011 ViPetroFF ViktorPetroFF@mail.ru

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}

unit FractalForest;

{$mode objfpc}{$H+}

interface

{Uses WinGraph Unit v1.1 (03-2010) - http://math.ubbcluj.ro/~sberinde/wingraph}
uses
  WinGraph, FractalTree;

type

  THardwoodTreeA=class(TTree)
  private
  protected

    procedure DrawTrunk(rLength, rPhi : Real; wLevel : Word=0); override;
    procedure DrawLeave(rLength, rPhi : Real; wLevel : Word=0); override;
    procedure DrawLeaveFractal(rLength, rPhi : Real; wLevel : Word=0); override;

  public
    constructor Create(lx, ly, lHigh : LongInt);

    procedure Dump; override;
  end;

  THardwoodTreeB=class(TTree)
  private
  protected

    procedure DrawTrunk(rLength, rPhi : Real; wLevel : Word=0); override;
    procedure DrawLeave(rLength, rPhi : Real; wLevel : Word=0); override;
    procedure DrawLeaveFractal(rLength, rPhi : Real; wLevel : Word=0); override;

  public
    constructor Create(lx, ly, lHigh : LongInt);

    procedure Dump; override;
  end;


  TAtomicTreeA=class(TTree)
  private
  protected

    procedure DrawTrunk(rLength, rPhi : Real; wLevel : Word=0); override;
    procedure DrawLeave(rLength, rPhi : Real; wLevel : Word=0); override;
    procedure DrawLeaveFractal(rLength, rPhi : Real; wLevel : Word=0); override;

  public
    constructor Create(lx, ly, lHigh : LongInt);

    procedure Dump; override;
  end;

  TAtomicTreeB=class(TAtomicTreeA)
  private
  protected

    procedure DrawTrunk(rLength, rPhi : Real; wLevel : Word=0); override;
    procedure DrawLeaveFractal(rLength, rPhi : Real; wLevel : Word=0); override;

  public

    procedure Dump; override;
  end;

  TConiferousTreeA=class(TTree)
  private
  protected

    procedure DrawTrunk(rLength, rPhi : Real; wLevel : Word=0); override;
    procedure DrawLeave(rLength, rPhi : Real; wLevel : Word=0); override;
    procedure DrawLeaveFractal(rLength, rPhi : Real; wLevel : Word=0); override;

  public
    constructor Create(lx, ly, lHigh : LongInt);

    procedure Dump; override;
  end;

  TConiferousTreeB=class(TConiferousTreeA)
  private
  protected
    procedure DrawLeaveFractal(rLength, rPhi : Real; wLevel : Word=0); override;

  public

    procedure Dump; override;
  end;


implementation

procedure THardwoodTreeA.DrawTrunk(rLength, rPhi : Real; wLevel : Word);
var rThickness : Real; color : LongWord; infoLn : LineSettingsType;
begin
  color:=GetColor; GetLineSettings(infoLn);
  rThickness:=4*High*MaxLevel; rThickness:=rThickness/(100.0*Succ(wLevel));
  SetColor(SaddleBrown); SetLineStyle(SolidLn, 0, Round(rThickness));
  LineRel(Round(rLength*cos(rPhi)), -Round(rLength*sin(rPhi)));
  SetColor(color); SetLineStyle(infoLn.linestyle, infoLn.pattern, infoLn.thickness);
end;

procedure THardwoodTreeA.DrawLeave(rLength, rPhi : Real; wLevel : Word);
var
  wR : Word;
  srcX, srcY : SmallInt;
  color : LongWord;
  rR : Real;
  fill : FillSettingsType;
begin
  color:=GetColor; GetFillSettings(fill);
  rR:=4*High*MaxLevel; rR:=rR/(100.0*Succ(wLevel));

  if rR < 2.0 then rR:=2.0;
  SetColor(DarkGreen); SetFillStyle(SolidFill, ForestGreen);
  wR:=Round(rR);
  srcX:=GetX+Round(rLength*cos(rPhi));
  srcY:=GetY-Round(rLength*sin(rPhi));
  FillEllipse(srcX, srcY, 2*wR, wR);
  SetColor(color); SetFillStyle(fill.pattern, fill.color);
end;

procedure THardwoodTreeA.DrawLeaveFractal(rLength, rPhi : Real; wLevel : Word);
var fromX, fromY : SmallInt; rNewLength : Real;
begin

  fromX:=GetX; fromY:=GetY;
  DrawTwig(rLength, rPhi, wLevel);

  if wLevel < MaxLevel then
  begin
      rNewLength:=rLength/Succ(wLevel)*0.78;
      fromX:=fromX+Round((rLength-rNewLength)*cos(rPhi));
      fromY:=fromY-Round((rLength-rNewLength)*sin(rPhi));
      MoveTo(fromX, fromY); DrawLeaveFractal(rNewLength, rPhi+Pi/8, Succ(wLevel));
      MoveTo(fromX, fromY); DrawLeaveFractal(rNewLength, rPhi-Pi/8, Succ(wLevel));
  end;
end;

constructor THardwoodTreeA.Create(lx, ly, lHigh : LongInt);
begin
  inherited Create(lx, ly, lHigh, 3);
end;

procedure THardwoodTreeA.Dump;
begin
  WriteLn('THardwoodTreeA.Dump');
end;


procedure THardwoodTreeB.DrawTrunk(rLength, rPhi : Real; wLevel : Word);
var rThickness : Real; color : LongWord; infoLn : LineSettingsType;
begin
  color:=GetColor; GetLineSettings(infoLn);
  rThickness:=rLength*0.12;
  SetColor(SaddleBrown); SetLineStyle(SolidLn, 0, Round(rThickness));
  LineRel(Round(rLength*cos(rPhi)), -Round(rLength*sin(rPhi)));
  SetColor(color); SetLineStyle(infoLn.linestyle, infoLn.pattern, infoLn.thickness);
end;

procedure THardwoodTreeB.DrawLeave(rLength, rPhi : Real; wLevel : Word);
var
  wR : Word;
  srcX, srcY : SmallInt;
  color : LongWord;
  rR : Real;
  fill : FillSettingsType;
begin

  if wLevel=MaxLevel then
  begin
    color:=GetColor; GetFillSettings(fill);
    rR:=8*High; rR:=rR/100.0;

    if rR < 2.0 then rR:=2.0;
    SetColor(DarkGreen); SetFillStyle(SolidFill, ForestGreen);
    wR:=Round(rR);
    srcX:=GetX+Round(rLength*cos(rPhi));
    srcY:=GetY-Round(rLength*sin(rPhi));
    FillEllipse(srcX, srcY, wR, wR);
    SetColor(color); SetFillStyle(fill.pattern, fill.color);
  end;
end;

procedure THardwoodTreeB.DrawLeaveFractal(rLength, rPhi : Real; wLevel : Word);
var fromX, fromY : SmallInt;
begin

  DrawTwig(rLength, rPhi, wLevel);
  fromX:=GetX; fromY:=GetY;

  if wLevel < MaxLevel then
  begin
      DrawLeaveFractal(rLength*0.7, rPhi+pi/4, Succ(wLevel));
      MoveTo(fromX, fromY);
      DrawLeaveFractal(rLength*0.7, rPhi-pi/6, Succ(wLevel));
  end;
end;

constructor THardwoodTreeB.Create(lx, ly, lHigh : LongInt);
begin
  inherited Create(lx, ly, lHigh, 8);
end;

procedure THardwoodTreeB.Dump;
begin
  WriteLn('THardwoodTreeB.Dump');
end;


procedure TAtomicTreeA.DrawTrunk(rLength, rPhi : Real; wLevel : Word);
var rThickness : Real; color : LongWord; infoLn : LineSettingsType;
begin
  color:=GetColor; GetLineSettings(infoLn);
  rThickness:=rLength*0.12;
  SetColor(SaddleBrown); SetLineStyle(SolidLn, 0, Round(rThickness));
  LineRel(Round(rLength*cos(rPhi)), -Round(rLength*sin(rPhi)));
  SetColor(color); SetLineStyle(infoLn.linestyle, infoLn.pattern, infoLn.thickness);
end;

procedure TAtomicTreeA.DrawLeave(rLength, rPhi : Real; wLevel : Word);
var
  wR : Word;
  srcX, srcY : SmallInt;
  color : LongWord;
  rR : Real;
  fill : FillSettingsType;
begin

  if wLevel=MaxLevel then
  begin
    color:=GetColor; GetFillSettings(fill);
    rR:=8*High; rR:=rR/100.0;

    if rR < 2.0 then rR:=2.0;
    SetColor(DarkGreen); SetFillStyle(SolidFill, ForestGreen);
    wR:=Round(rR);
    srcX:=GetX+Round(rLength*cos(rPhi));
    srcY:=GetY-Round(rLength*sin(rPhi));

    PieSlice(srcX, srcY, Round((rPhi-Pi/6)*360/(2*pi)), Round((rPhi+Pi/6)*360/(2*pi)), wR);
    PieSlice(srcX, srcY, Round((rPhi-Pi/6+2*pi/3)*360/(2*pi)), Round((rPhi+Pi/6+2*pi/3)*360/(2*pi)), wR);
    PieSlice(srcX, srcY, Round((rPhi-Pi/6-2*pi/3)*360/(2*pi)), Round((rPhi+Pi/6-2*pi/3)*360/(2*pi)), wR);
    SetColor(color); SetFillStyle(fill.pattern, fill.color);
  end;
end;

procedure TAtomicTreeA.DrawLeaveFractal(rLength, rPhi : Real; wLevel : Word);
var fromX, fromY : SmallInt;
begin

  DrawTwig(rLength, rPhi, wLevel);

  if wLevel < MaxLevel then
  begin
      fromX:=GetX; fromY:=GetY;
      DrawLeaveFractal(rLength*0.4, rPhi+14*pi/30, Succ(wLevel));
      MoveTo(fromX, fromY);
      DrawLeaveFractal(rLength*0.4, rPhi-14*pi/30, Succ(wLevel));
      MoveTo(fromX, fromY);
      DrawLeaveFractal(rLength*0.7, rPhi+Pi/30, Succ(wLevel));
  end;
end;

constructor TAtomicTreeA.Create(lx, ly, lHigh : LongInt);
begin
  inherited Create(lx, ly, lHigh);
end;

procedure TAtomicTreeA.Dump;
begin
  WriteLn('TAtomicTreeA.Dump');
end;


procedure TAtomicTreeB.DrawTrunk(rLength, rPhi : Real; wLevel : Word);
begin
  inherited DrawTrunk(rLength, rPhi, wLevel);
end;


procedure TAtomicTreeB.DrawLeaveFractal(rLength, rPhi : Real; wLevel : Word);
var fromX, fromY : SmallInt;
begin

  DrawTwig(rLength, rPhi, wLevel);

  if wLevel < MaxLevel then
  begin
      fromX:=GetX; fromY:=GetY;
      DrawLeaveFractal(rLength*0.4, rPhi-pi/4, Succ(wLevel));
      MoveTo(fromX+Round(rLength*0.4*cos(rPhi)), fromY-Round(rLength*0.4*sin(rPhi)));
      DrawLeaveFractal(rLength*0.4, rPhi+pi/4, Succ(wLevel));
      MoveTo(fromX, fromY);
      DrawLeaveFractal(rLength*0.7, rPhi, Succ(wLevel));
  end;
end;

procedure TAtomicTreeB.Dump;
begin
  WriteLn('TAtomicTreeB.Dump');
end;


procedure TConiferousTreeA.DrawTrunk(rLength, rPhi : Real; wLevel : Word);
var rThickness : Real; color : LongWord; infoLn : LineSettingsType;
begin
  color:=GetColor; GetLineSettings(infoLn);
  if wLevel > (MaxLevel-2) then
  begin
    SetColor(DarkGreen); SetFillStyle(SolidFill, ForestGreen);
  end else
  begin
    rThickness:=rLength*0.08;
    SetColor(SaddleBrown); SetLineStyle(SolidLn, 0, Round(rThickness));
  end;
  LineRel(Round(rLength*cos(rPhi)), -Round(rLength*sin(rPhi)));
  SetColor(color); SetLineStyle(infoLn.linestyle, infoLn.pattern, infoLn.thickness);
end;

procedure TConiferousTreeA.DrawLeave(rLength, rPhi : Real; wLevel : Word);
var
  color : LongWord;
  fill : FillSettingsType;
begin

  if wLevel=MaxLevel then
  begin
    color:=GetColor; GetFillSettings(fill);

    SetColor(DarkGreen); SetFillStyle(SolidFill, ForestGreen);
    color:=GetColor;
    MoveRel(Round(rLength*cos(rPhi)), Round(-rLength*sin(rPhi)));
    PutPixel(GetX+1, GetY+1, color); PutPixel(GetX-1, GetY-1, color);
    PutPixel(GetX+1, GetY-1, color); PutPixel(GetX-1, GetY+1, color);
    SetColor(color); SetFillStyle(fill.pattern, fill.color);
  end;
end;

procedure TConiferousTreeA.DrawLeaveFractal(rLength, rPhi : Real; wLevel : Word);
var fromX, fromY : SmallInt;
begin

  DrawTwig(rLength, rPhi, wLevel);
  fromX:=GetX; fromY:=GetY;

  if wLevel < MaxLevel then
  begin
      DrawLeaveFractal(rLength*0.33, rPhi+pi/2+pi/6, Succ(wLevel));
      MoveTo(fromX, fromY);
      DrawLeaveFractal(rLength*0.33, rPhi-pi/2-pi/6, Succ(wLevel));
      MoveTo(fromX, fromY);
      DrawLeaveFractal(rLength*0.77, rPhi, Succ(wLevel));
  end;
end;

constructor TConiferousTreeA.Create(lx, ly, lHigh : LongInt);
begin
  inherited Create(lx, ly, lHigh, 7);
end;

procedure TConiferousTreeA.Dump;
begin
  WriteLn('TConiferousTreeA.Dump');
end;


procedure TConiferousTreeB.DrawLeaveFractal(rLength, rPhi : Real; wLevel : Word);
var fromX, fromY : SmallInt;
begin

  DrawTwig(rLength, rPhi, wLevel);

  if wLevel < MaxLevel then
  begin
      fromX:=GetX; fromY:=GetY;
      MoveTo(fromX+Round(rLength*0.4*cos(rPhi)), fromY-Round(rLength*0.4*sin(rPhi)));
      DrawLeaveFractal(rLength*0.4, rPhi-pi/4, Succ(wLevel));
      MoveTo(fromX, fromY);
      DrawLeaveFractal(rLength*0.4, rPhi+pi/4, Succ(wLevel));
      MoveTo(fromX, fromY);
      DrawLeaveFractal(rLength*0.7, rPhi-pi/30, Succ(wLevel));
  end;
end;


procedure TConiferousTreeB.Dump;
begin
  WriteLn('TConiferousTreeB.Dump');
end;

end.

