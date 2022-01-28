unit Settings;

interface

function get(section,key : String) : String;

procedure put(section,key,value : String);

implementation
uses sysutils, inifiles;
var
   IniFile : TIniFile;

   function get(section,key : String) : String;
   begin
     get := iniFile.ReadString(section,key,'');
   end;

   procedure put(section,key,value : String);
   begin
     iniFile.WriteString(section,key,value);
   end;

initialization

  iniFile := TIniFile.Create(ChangeFileExt(ParamStr(0),'.ini')) ;

finalization
  iniFile.Free;
end.
