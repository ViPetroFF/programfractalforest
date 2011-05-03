{ Sample Program # 12, Source: FractalTree.pas, Date: 04.05.2011 0:15:10

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

unit FractalTree;

{$mode objfpc}{$H+}
{$static on}

interface

{Uses WinGraph Unit v1.1 (03-2010) - http://math.ubbcluj.ro/~sberinde/wingraph}
uses
  Objects, SysUtils, WinGraph;

type
  TTreeBase=class;
  TTreeBase=class
  private
    s_Head, s_Last : TTreeBase; static;
    _Next : TTreeBase;

  public
    class function Head : TTreeBase;
    class function Last : TTreeBase;
    class procedure DestroyAll;
    class procedure ViewAll(ViewProc: Pointer);
    class procedure DrawAll;

    constructor Initialize;
    constructor Create;
    destructor Destroy; override;

    function GetHead : TTreeBase;
    function GetLast : TTreeBase;

    procedure Draw; virtual;
    procedure Dump; virtual;

    property Next : TTreeBase read _Next;
  end;

  TTree=class(TTreeBase)
  private
    _timeStart : TDateTime;
    _RootTreeXY : PointType;
    _High : Real;
    _MaxLevel : Word;

  protected
    procedure DrawTwig(rLength, rPhi : Real; wLevel : Word=0);

    function  GetHigh : Real; virtual;
    procedure DrawTrunk(rLength, rPhi : Real; wLevel : Word=0); virtual;
    procedure DrawLeave(rLength, rPhi : Real; wLevel : Word=0); virtual;
    procedure DrawLeaveFractal(rLength, rPhi : Real; wLevel : Word=0); virtual;

  public
    constructor Create(lx, ly, lHigh : LongInt; wMaxLevel : Word=4);
    destructor Destroy; override;

    procedure Draw; override;
    procedure Dump; override;

    property StartTime : TDateTime read _timeStart;
    property X : LongInt read _RootTreeXY.x write _RootTreeXY.x;
    property Y : LongInt read _RootTreeXY.y write _RootTreeXY.y;
    property High : Real read GetHigh;
    property MaxLevel : Word read _MaxLevel write _MaxLevel;
  end;

implementation

class function TTreeBase.Head : TTreeBase;
begin
  Head:=TTreeBase(Self).GetHead;
end;

class function TTreeBase.Last : TTreeBase;
begin
  Last:=TTreeBase(Self).GetLast;
end;

class procedure TTreeBase.DestroyAll;
var
  Queue : TTreeBase;
begin
  Queue:=TTreeBase(Self);
  while not (Queue.GetHead = nil) do
    begin
      Queue.GetHead.Destroy;
    end;
end;

class procedure TTreeBase.ViewAll(ViewProc: Pointer);
type
  TViewProc = procedure(Entry : TTreeBase);
var
  Queue, Current : TTreeBase;
  fnViewProc : TViewProc;
begin
  Queue:=TTreeBase(Self);

  Current:=Queue.GetHead;
  fnViewProc:=TViewProc(ViewProc);
  while not (Current = nil) do
    begin
      {fnViewProc(Current);}
      CallPointerLocal(fnViewProc,get_caller_frame(get_frame),Current);
      Current := Current.Next;
    end;
end;

class procedure TTreeBase.DrawAll;

  procedure DrawProc (ptr: Pointer);{$IFNDEF FPC}FAR;{$ENDIF}
  begin
    TTreeBase(ptr).Draw;
  end;

begin
  ViewAll(@DrawProc);
end;

constructor TTreeBase.Initialize;
begin
  inherited Create;
  s_Head:=nil;
  s_Last:=s_Head;
end;


constructor TTreeBase.Create;
begin
  inherited Create;

  _Next:=nil;

   if s_Head = nil then
      s_Head:=Self
   else
      s_Last._Next:=Self;

   s_Last:=Self;
end;

destructor TTreeBase.Destroy;
begin
  inherited Destroy;

   if not (s_Head = nil) and (s_Head = Self) then
     s_Head:= s_Head.Next;
end;

function TTreeBase.GetHead : TTreeBase;
begin
  GetHead:=s_Head;
end;

function TTreeBase.GetLast : TTreeBase;
begin
  GetLast:=s_Last;
end;

procedure TTreeBase.Draw;
begin
  AbstractError;
end;

procedure TTreeBase.Dump;
begin
  WriteLn('TTreeBase.Dump ', (s_Head = nil));
end;

function TTree.GetHigh : Real;
var nowTime : LongInt;
begin
  nowTime:=DateTimeToFileDate(Now);
  GetHigh:=Round(_High+(_High/50)*(Succ(nowTime)-DateTimeToFileDate(StartTime)));
end;

procedure TTree.DrawTwig(rLength, rPhi : Real; wLevel : Word);
var fromX, fromY, toX, toY : SmallInt;
begin
  fromX:=GetX; fromY:=GetY;
  DrawTrunk(rLength, rPhi, wLevel);
  toX:=GetX; toY:=GetY;
  MoveTo(fromX, fromY);
  DrawLeave(rLength, rPhi, wLevel);
  MoveTo(toX, toY);
end;

procedure TTree.DrawTrunk(rLength, rPhi : Real; wLevel : Word);
begin
  LineRel(Round(rLength*cos(rPhi)), Round(-rLength*sin(rPhi)));
end;

procedure TTree.DrawLeave(rLength, rPhi : Real; wLevel : Word);
var color : LongWord;
begin

  if wLevel=MaxLevel then
  begin
    color:=GetColor;
    MoveRel(Round(rLength*cos(rPhi)), Round(-rLength*sin(rPhi)));
    PutPixel(GetX+1, GetY+1, color); PutPixel(GetX-1, GetY-1, color);
    PutPixel(GetX+1, GetY-1, color); PutPixel(GetX-1, GetY+1, color);
  end;
end;

procedure TTree.DrawLeaveFractal(rLength, rPhi : Real; wLevel : Word);
var fromX, fromY : SmallInt;
begin

  DrawTwig(rLength, rPhi, wLevel);
  fromX:=GetX; fromY:=GetY;

  if wLevel < MaxLevel then
  begin
      DrawLeaveFractal(rLength*0.33, rPhi+pi/4, Succ(wLevel));
      MoveTo(fromX, fromY);
      DrawLeaveFractal(rLength*0.33, rPhi-pi/4, Succ(wLevel));
      MoveTo(fromX, fromY);
      DrawLeaveFractal(rLength*0.66, rPhi, Succ(wLevel));
  end;
end;

constructor TTree.Create(lx, ly, lHigh : LongInt; wMaxLevel : Word);
begin
  inherited Create;

  MaxLevel:=wMaxLevel;
  _timeStart:=Now;
  X:=lx; Y:=ly; _High:=lHigh;
end;

destructor TTree.Destroy;
begin
  inherited Destroy;
end;

procedure TTree.Draw;
begin
    MoveTo(X,Y); DrawLeaveFractal(High, Pi/2);
end;

procedure TTree.Dump;
begin
  WriteLn('TTree.Dump');
end;

end.

