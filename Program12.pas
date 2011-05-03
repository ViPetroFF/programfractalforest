{ Sample Program # 12, Source: Program12.pas, Date: 04.05.2011 0:11:55

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

program Program12;

{$mode objfpc}{$H+}

{Uses WinGraph Unit v1.1 (03-2010) - http://math.ubbcluj.ro/~sberinde/wingraph}
uses
  SysUtils,WinGraph,FractalTree,FractalForest;

{$IFDEF WINDOWS}{$R Program12.rc}{$ENDIF}


var
  Forest : TTreeBase;
  Gd, Gm : SmallInt;

  ndx : Integer;
  rndX: Byte;
  setX : set of Byte;
  pageN : Byte;

begin
  Forest:=TTreeBase.Initialize;
  Forest.Destroy;

  Randomize;
  {Gd:=Detect;}
  DetectGraph(Gd,Gm);

  Gm:=mMaximized;
  InitGraph(Gd, Gm, '');
  If GraphResult <> grOk Then Halt(1);

  pageN:=0;

  setX:=[0];
  Include(setX, GetMaxX div 80);

  {Forest:=TAtomicTreeB.Create(GetMaxX div 2,GetMaxY,250);}

  for ndx:=1 to 3 do
  begin
    rndX:=Random(GetMaxX div 80);
    if not (rndX in setX) then
    begin
      Forest:=TAtomicTreeA.Create(rndX*80,GetMaxY,12+Random(8));
      Include(setX, rndX);
    end;

    rndX:=Random(GetMaxX div 80);
    if not (rndX in setX) then
    begin
      Forest:=TAtomicTreeB.Create(rndX*80,GetMaxY,12+Random(8));
      Include(setX, rndX);
    end;

    rndX:=Random(GetMaxX div 80);
    if not (rndX in setX) then
    begin
      Forest:=TConiferousTreeA.Create(rndX*80,GetMaxY,12+Random(8));
      Include(setX, rndX);
    end;

    rndX:=Random(GetMaxX div 80);
    if not (rndX in setX) then
    begin
      Forest:=TConiferousTreeB.Create(rndX*80,GetMaxY,12+Random(8));
      Include(setX, rndX);
    end;

    rndX:=Random(GetMaxX div 80);
    if not (rndX in setX) then
    begin
      Forest:=THardwoodTreeA.Create(rndX*80,GetMaxY,50+Random(40));
      Include(setX, rndX);
    end;

    rndX:=Random(GetMaxX div 80);
    if not (rndX in setX) then
    begin
      Forest:=THardwoodTreeB.Create(rndX*80,GetMaxY,6+Random(4));
      Include(setX, rndX);
    end;
  end;

  repeat
    setvisualpage(1-pageN);
    setactivepage(pageN);
    Sleep(100);

    UpdateGraph(UpdateOff);
    cleardevice;
    Forest.DrawAll;
    UpdateGraph(UpdateNow);
    pageN:=(1-pageN);
  until CloseGraphRequest;

  Forest.DestroyAll;
  CloseGraph;
end.

