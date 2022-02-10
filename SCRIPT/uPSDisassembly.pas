unit uPSDisassembly;
{$I PascalScript.inc}

interface

uses
  ps_runtime, ps_utils, SysUtils, ps_defs;

//==============================================================================
//
// IFPS3DataToText
//
//==============================================================================
function IFPS3DataToText(const Input: tbtstring; var Output: string): Boolean;

implementation

uses
  ps_import;

type
  TMyPSExec = class(TDoomExec)
    function ImportProc(const Name: ShortString; proc: TIFExternalProcRec): Boolean; override;
  end;

//==============================================================================
//
// Debug2Str
//
//==============================================================================
function Debug2Str(const s: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(s) do
  begin
    if (s[i] < #32) or (s[i] > #128) then
      Result := Result + '\' + IntToHex(Ord(s[i]), 2)
    else if s[i] = '\' then
      Result := Result + '\\'
    else
      Result := Result + s[i];
  end;
end;

//==============================================================================
//
// SpecImportProc
//
//==============================================================================
function SpecImportProc(Sender: TObject; p: TIFExternalProcRec): Boolean; forward;

//==============================================================================
//
// MyFloatToStr
//
//==============================================================================
function MyFloatToStr(Value: Extended): string;
begin
  try
    Result := SysUtils.FloatToStr(Value);
  except
    Result := 'NaNa';
  end;
end;

//==============================================================================
//
// IFPS3DataToText
//
//==============================================================================
function IFPS3DataToText(const Input: tbtstring; var Output: string): Boolean;
var
  I: TMyPSExec;

  procedure WriteOutPut(const s: string);
  begin
    Output := Output + s + #13#10;
  end;

  function BT2S(P: PIFTypeRec): string;
  var
    i: Longint;
  begin
    case p.BaseType of
      btU8: Result := 'U8';
      btS8: Result := 'S8';
      btU16: Result := 'U16';
      btS16: Result := 'S16';
      btU32: Result := 'U32';
      btS32: Result := 'S32';
      {$IFNDEF PS_NOINT64}
      bts64: Result := 'S64';
      {$ENDIF}
      btChar: Result := {$IFDEF PS_PANSICHAR}'AnsiChar'{$ELSE}'Char'{$ENDIF};
      {$IFNDEF PS_NOWIDESTRING}
      btWideChar: Result := 'WideChar';
      btWideString: Result := 'WideString';
      {$ENDIF}
      btSet: Result := 'Set';
      btSingle: Result := 'Single';
      btDouble: Result := 'Double';
      btExtended: Result := 'Extended';
      btString: Result := 'String';
      btRecord:
        begin
          Result := 'Record(';
          for i := 0 to TPSTypeRec_Record(p).FieldTypes.Count-1 do
          begin
            if i <> 0 then
              Result := Result + ',';
            Result := Result + BT2S(PIFTypeRec(TPSTypeRec_Record(p).FieldTypes[i]));
          end;
          Result := Result + ')';
        end;
      btArray: Result := 'Array of ' + BT2S(TPSTypeRec_Array(p).ArrayType);
      btResourcePointer: Result := 'ResourcePointer';
      btPointer: Result := 'Pointer';
      btVariant: Result := 'Variant';
      btClass: Result := 'Class';
      btProcPtr: Result := 'ProcPtr';
      btStaticArray: Result := 'StaticArray[' + IntToStr(TPSTypeRec_StaticArray(p).Size) + '] of ' + BT2S(TPSTypeRec_Array(p).ArrayType);
      btPChar: Result := 'PChar';
      btCurrency: Result := 'Currency';
      btUnicodeString: Result := 'UnicodeString';
      btInterface: Result := 'Interface';
      btType: Result := 'Type';
      btEnum: Result := 'Enum';
      btExtClass: Result := 'ExtClass';
    else
      Result := 'Unknown ' + IntToStr(p.BaseType);
    end;
  end;

  procedure WriteTypes;
  var
    T: Longint;
  begin
    WriteOutPut('[TYPES]');
    for T := 0 to i.FTypes.Count -1 do
    begin
      if PIFTypeRec(i.FTypes[t]).ExportName <> '' then
        WriteOutPut('Type [' + IntToStr(t) + ']: ' + BT2S(PIFTypeRec(i.FTypes[t])) + ' Export: ' + PIFTypeRec(i.FTypes[t]).ExportName)
      else
        WriteOutPut('Type [' + IntToStr(t) + ']: ' + BT2S(PIFTypeRec(i.FTypes[t])));
    end;
  end;

  procedure WriteVars;
  var
    T: Longint;

    function FindType(p: Pointer): Cardinal;
    var
      T: Longint;
    begin
      Result := Cardinal(-1);
      for T := 0 to i.FTypes.Count -1 do
      begin
        if p = i.FTypes[t] then
        begin
          Result := t;
          Exit;
        end;
      end;
    end;

  begin
    WriteOutPut('[VARS]');
    for t := 0 to i.FGlobalVars.count -1 do
    begin
      WriteOutPut('Var [' + IntToStr(t) + ']: ' + IntToStr(FindType(PIFVariant(i.FGlobalVars[t])^.FType)) + ' ' + BT2S(PIFVariant(i.FGlobalVars[t])^.Ftype) + ' ' + PIFVariant(i.FGlobalVars[t])^.Ftype.ExportName);
    end;
  end;

  procedure WriteProcs;
  var
    t: Longint;

    procedure WriteProc(proc: TPSProcRec);
    var
      sc, CP: Cardinal;
      function ReadData(var Data; Len: Cardinal): Boolean;
      begin
        if CP + Len <= TPSInternalProcRec(PROC).Length then
        begin
          Move(TPSInternalProcRec(Proc).Data[CP], Data, Len);
          CP := CP + Len;
          Result := True;
        end
        else
          Result := False;
      end;

      function ReadByte(var B: Byte): Boolean;
      begin
        if CP < TPSInternalProcRec(Proc).Length then
        begin
          b := TPSInternalProcRec(Proc).Data^[CP];
          Inc(CP);
          Result := True;
        end
        else
          Result := False;
      end;

      function ReadLong(var B: Cardinal): Boolean;
      begin
        if CP + 3 < TPSInternalProcRec(Proc).Length then
        begin
          b := Cardinal((@TPSInternalProcRec(Proc).Data[CP])^);
          Inc(CP, 4);
          Result := True;
        end
        else
          Result := False;
      end;

      function ReadWriteVariable: string;
      var
        VarType: byte;
        L1, L2: Cardinal;

        function ReadVar(FType: Cardinal): string;
        var
          F: PIFTypeRec;
          b: byte;
          w: word;
          l: Cardinal;
          {$IFNDEF PS_NOINT64}
          ff: Int64;
          {$ENDIF}
          e: extended;
          ss: single;
          d: double;
          s: ansistring;
          c: char;
          {$IFNDEF PS_NOWIDESTRING}
          wc: WideChar;
          ws: WideString;
          {$ENDIF}
        begin
          Result := '';
          F:= i.FTypes[Ftype];
          if f = nil then
            Exit;
          case f.BaseType of
            btProcPtr:
              begin
                if not ReadData(l, 4) then
                  Exit;
                Result := 'PROC: ' + IntToStr(l);
              end;
            btU8:
              begin
                if not ReadData(b, 1) then
                  Exit;
                Result := IntToStr(tbtu8(B));
              end;
            btS8:
              begin
                if not ReadData(b, 1) then
                  Exit;
                Result := IntToStr(tbts8(B));
              end;
            btU16:
              begin
                if not ReadData(w, 2) then
                  Exit;
                Result := IntToStr(tbtu16(w));
              end;
            btS16:
              begin
                if not ReadData(w, 2) then
                  Exit;
                Result := IntToStr(tbts16(w));
              end;
            btU32:
              begin
                if not ReadData(l, 4) then
                  Exit;
                Result := IntToStr(tbtu32(l));
              end;
            btS32:
              begin
                if not ReadData(l, 4) then
                  Exit;
                Result := IntToStr(tbts32(l));
              end;
            {$IFNDEF PS_NOINT64}
            bts64:
              begin
                if not ReadData(ff, 8) then
                  Exit;
                Result := IntToStr(ff);
              end;
            {$ENDIF}
            btSingle:
              begin
                if not ReadData(ss, Sizeof(tbtsingle)) then
                  Exit;
                Result := MyFloatToStr(ss);
              end;
            btDouble:
              begin
                if not ReadData(d, Sizeof(tbtdouble)) then
                  Exit;
                Result := MyFloatToStr(d);
              end;
            btExtended:
              begin
                if not ReadData(e, Sizeof(tbtextended)) then
                  Exit;
                Result := MyFloatToStr(e);
              end;
            btPChar,
            btString:
              begin
                if not ReadData(l, 4) then
                  Exit;
                SetLength(s, l);
                if not ReadData(s[1], l) then
                  Exit;
                Result := MakeString(s);
              end;
            btSet:
              begin
                SetLength(s, TPSTypeRec_Set(f).aByteSize);
                if not ReadData(s[1], Length(s)) then
                  Exit;
                Result := MakeString(s);
              end;
            btChar:
              begin
                if not ReadData(c, 1) then
                  Exit;
                Result := '#' + IntToStr(Ord(c));
              end;
            {$IFNDEF PS_NOWIDESTRING}
            btWideChar:
              begin
                if not ReadData(wc, 2) then
                  Exit;
                Result := '#' + IntToStr(Ord(wc));
              end;
            btWideString:
              begin
                if not ReadData(l, 4) then
                  Exit;
                SetLength(ws, l);
                if not ReadData(ws[1], l * 2) then
                  Exit;
                Result := MakeWString(ws);
              end;
            {$ENDIF}
          end;
        end;

        function AddressToStr(a: Cardinal): String;
        begin
          if a < PSAddrNegativeStackStart then
            Result := 'GlobalVar[' + IntToStr(a) + ']'
          else
            Result := 'Base[' + IntToStr(Longint(A - PSAddrStackStart)) + ']';
        end;

      begin
        Result := '';
        if not ReadByte(VarType) then
          Exit;
        case VarType of
          0:
            begin
              if not ReadLong(L1) then
                Exit;
              Result := AddressToStr(L1);
            end;
          1:
            begin
              if not ReadLong(L1) then
                Exit;
              Result := '[' + ReadVar(l1) + ']';
            end;
          2:
            begin
              if not ReadLong(L1) then
                Exit;
              if not ReadLong(L2) then
                Exit;
              Result := AddressToStr(L1) + '.[' + IntToStr(l2) + ']';
            end;
          3:
            begin
              if not ReadLong(l1) then
                Exit;
              if not ReadLong(l2) then
                Exit;
              Result := AddressToStr(L1) + '.' + AddressToStr(l2);
            end;
        end;
      end;

    var
      b: Byte;
      s: string;
      DP, D1, D2, D3, D4: Cardinal;
    begin
      CP := 0;
      sc := 0;
      while True do
      begin
        DP := CP;
        if not ReadByte(b) then
          Exit;
        case b of
          CM_A:
            begin
              {$IFDEF FPC}
              Output := Output + ' [' + IntToStr(DP) + '] ASSIGN ' + ReadWriteVariable;
              Output := Output + ', ' + ReadWriteVariable + #13#10;
              {$ELSE}
              WriteOutPut(' [' + IntToStr(DP) + '] ASSIGN ' + ReadWriteVariable+ ', ' + ReadWriteVariable);
              {$ENDIF}
            end;
          CM_CA:
            begin
              if not ReadByte(b) then
                Exit;
              case b of
                0: s:= '+';
                1: s := '-';
                2: s := '*';
                3: s:= '/';
                4: s:= 'MOD';
                5: s:= 'SHL';
                6: s:= 'SHR';
                7: s:= 'AND';
                8: s:= 'OR';
                9: s:= 'XOR';
              else
                Exit;
              end;
              WriteOutPut(' [' + IntToStr(DP) + '] CALC ' + ReadWriteVariable + ' ' + s + ' ' + ReadWriteVariable);
            end;
          CM_P:
            begin
              Inc(sc);
              WriteOutPut(' [' + IntToStr(DP) + '] PUSH ' + ReadWriteVariable + ' // ' + IntToStr(sc));
            end;
          CM_PV:
            begin
              Inc(sc);
              WriteOutPut(' [' + IntToStr(DP) + '] PUSHVAR ' + ReadWriteVariable + ' // ' + IntToStr(sc));
            end;
          CM_PO:
            begin
              Dec(Sc);
              WriteOutPut(' [' + IntToStr(DP) + '] POP // ' + IntToStr(sc));
            end;
          Cm_C:
            begin
              if not ReadLong(D1) then
                Exit;
              WriteOutPut(' [' + IntToStr(DP) + '] CALL ' + IntToStr(D1));
            end;
          CM_PG:
            begin
              if not ReadLong(D1) then
                Exit;
              WriteOutPut(' [' + IntToStr(DP) + '] POP/GOTO currpos + ' + IntToStr(D1) + ' [' + IntToStr(CP + D1) + ']');
            end;
          CM_P2G:
            begin
              if not ReadLong(D1) then
                Exit;
              WriteOutPut(' [' + IntToStr(DP) + '] POP2/GOTO currpos + ' + IntToStr(D1) + ' [' + IntToStr(CP + D1) + ']');
            end;
          CM_G:
            begin
              if not ReadLong(D1) then
                Exit;
              WriteOutPut(' [' + IntToStr(DP) + '] GOTO currpos + ' + IntToStr(D1) + ' [' + IntToStr(CP + D1) + ']');
            end;
          CM_CG:
            begin
              if not ReadLong(D1) then
                Exit;
              WriteOutPut(' [' + IntToStr(DP) + '] COND_GOTO currpos + ' + IntToStr(D1) + ' ' + ReadWriteVariable + ' [' + IntToStr(CP + D1) + ']');
            end;
          CM_CNG:
            begin
              if not ReadLong(D1) then
                Exit;
              WriteOutPut(' [' + IntToStr(DP) + '] COND_NOT_GOTO currpos + ' + IntToStr(D1) + ' ' + ReadWriteVariable + ' [' + IntToStr(CP + D1) + ']');
            end;
          CM_R: WriteOutPut(' [' + IntToStr(DP) + '] RET');
          CM_ST:
            begin
              if not ReadLong(D1) or not ReadLong(D2) then
                Exit;
              WriteOutPut(' [' + IntToStr(DP) + '] SETSTACKTYPE Base[' + IntToStr(D1) + '] ' + IntToStr(D2));
            end;
          CM_PT:
            begin
              Inc(sc);
              if not ReadLong(D1) then
                Exit;
              WriteOutPut(' [' + IntToStr(DP) + '] PUSHTYPE ' + IntToStr(D1) + '(' + BT2S(TPSTypeRec(I.FTypes[D1])) + ') // ' + IntToStr(sc));
            end;
          CM_CO:
            begin
              if not ReadByte(b) then
                Exit;
              case b of
                0: s := '>=';
                1: s := '<=';
                2: s := '>';
                3: s := '<';
                4: s := '<>';
                5: s := '=';
                else Exit;
              end;
              WriteOutPut(' [' + IntToStr(DP) + '] COMPARE into ' + ReadWriteVariable + ': ' + ReadWriteVariable + ' ' + s + ' ' + ReadWriteVariable);
            end;
          CM_CV:
            begin
              WriteOutPut(' [' + IntToStr(DP) + '] CALLVAR ' + ReadWriteVariable);
            end;
          CM_INC:
            begin
              WriteOutPut(' [' + IntToStr(DP) + '] INC ' + ReadWriteVariable);
            end;
          CM_DEC:
            begin
              WriteOutPut(' [' + IntToStr(DP) + '] DEC ' + ReadWriteVariable);
            end;
          CM_SP:
            begin
              WriteOutPut(' [' + IntToStr(DP) + '] SETPOINTER ' + ReadWriteVariable + ': ' + ReadWriteVariable);
            end;
          CM_SPC:
            begin
              WriteOutPut(' [' + IntToStr(DP) + '] SETCOPYPOINTER ' + ReadWriteVariable + ': ' + ReadWriteVariable);
            end;
          CM_IN:
            begin
              WriteOutPut(' [' + IntToStr(DP) + '] INOT ' + ReadWriteVariable);
            end;
          CM_BN:
            begin
              WriteOutPut(' [' + IntToStr(DP) + '] BNOT ' + ReadWriteVariable);
            end;
          CM_VM:
            begin
              WriteOutPut(' [' + IntToStr(DP) + '] MINUS ' + ReadWriteVariable);
            end;
          CM_SF:
            begin
              s := ReadWriteVariable;
              if not ReadByte(b) then
                Exit;
              if b = 0 then
                WriteOutPut(' [' + IntToStr(DP) + '] SETFLAG ' + s)
              else
                WriteOutPut(' [' + IntToStr(DP) + '] SETFLAG NOT ' + s);
            end;
          CM_FG:
            begin
              if not ReadLong(D1) then
                Exit;
              WriteOutPut(' [' + IntToStr(DP) + '] FLAGGOTO currpos + ' + IntToStr(D1) + ' [' + IntToStr(CP + D1) + ']');
            end;
          CM_PUEXH:
            begin
              if not ReadLong(D1) then
                Exit;
              if not ReadLong(D2) then
                Exit;
              if not ReadLong(D3) then
                Exit;
              if not ReadLong(D4) then
                Exit;
              WriteOutPut(' [' + IntToStr(DP) + '] PUSHEXCEPTION ' + IntToStr(D1) + ' ' + IntToStr(D2) + ' ' + IntToStr(D3) + ' ' + IntToStr(D4));
            end;
          cm_poexh:
            begin
              if not ReadByte(b) then
                Exit;
              WriteOutPut(' [' + IntToStr(DP) + '] POPEXCEPTION ' + IntToStr(b));
            end;
        else
          begin
            WriteOutPut(' Disasm Error');
            Break;
          end;
        end;
      end;
    end;

  begin
    WriteOutPut('[PROCS]');
    for t := 0 to i.FProcs.Count -1 do
    begin
      if TPSProcRec(i.FProcs[t]).ClassType = TIFExternalProcRec then
      begin
        if TPSExternalProcRec(i.FProcs[t]). Decl = '' then
          WriteOutPut('Proc [' + IntToStr(t) + ']: External: ' + TPSExternalProcRec(i.FProcs[t]).Name)
        else
          WriteOutPut('Proc [' + IntToStr(t) + ']: External Decl: ' + Debug2Str(TIFExternalProcRec(i.FProcs[t]).Decl) + ' ' + TIFExternalProcRec(i.FProcs[t]).Name);
      end
      else
      begin
        if TPSInternalProcRec(i.FProcs[t]).ExportName <> '' then
        begin
          WriteOutPut('Proc [' + IntToStr(t) + '] Export: ' + TPSInternalProcRec(i.FProcs[t]).ExportName + ' ' + TPSInternalProcRec(i.FProcs[t]).ExportDecl);
        end
        else
          WriteOutPut('Proc [' + IntToStr(t) + ']');
        Writeproc(i.FProcs[t]);
      end;
    end;
  end;

begin
  Result := False;
  try
    I := TMyPSExec.Create;
    I.AddSpecialProcImport('', @SpecImportProc, nil);

    if not I.LoadData(Input) then
    begin
      I.Free;
      Exit;
    end;
    Output := '';
    WriteTypes;
    WriteVars;
    WriteProcs;
    I.Free;
  except
    Exit;
  end;
  Result := True;
end;

{ TMyIFPSExec }

//==============================================================================
//
// MyDummyProc
//
//==============================================================================
function MyDummyProc(Caller: TPSExec; p: TIFExternalProcRec; Global, Stack: TPSStack): Boolean;
begin
  Result := False;
end;

//==============================================================================
//
// TMyPSExec.ImportProc
//
//==============================================================================
function TMyPSExec.ImportProc(const Name: ShortString;
  proc: TIFExternalProcRec): Boolean;
begin
  Proc.ProcPtr := MyDummyProc;
  Result := True;
end;

//==============================================================================
//
// SpecImportProc
//
//==============================================================================
function SpecImportProc(Sender: TObject; p: TIFExternalProcRec): Boolean;
begin
  p.ProcPtr := MyDummyProc;
  Result := True;
end;

end.

