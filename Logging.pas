unit Logging;

{$mode objfpc}{$H+}

interface

uses
  MultiLog{$IFNDEF NoLogging}, IPCChannel{$ENDIF};

const
  lcAll = [0..31]; //all logging classes
  lcDebug = 0;
  lcError = 1;
  lcInfo = 2;
  lcWarning = 3;

  lcEvents = 4;

  lcServer = 10;
  lcClient = 11;
  lcLandscape = 12;

var
  Logger: TLogger;

implementation

initialization
  Logger := TLogger.Create;
  {$IFNDEF NoLogging}
  Logger.Channels.Add(TIPCChannel.Create);
  Logger.ActiveClasses := lcAll;
  {$ENDIF}

finalization
  Logger.Free;

end.

