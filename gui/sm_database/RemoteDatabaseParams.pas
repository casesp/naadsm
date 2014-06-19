unit RemoteDatabaseParams;

(*
RemoteDatabaseParams.pas
------------------------
Begin: 2007/03/19
Last revision: $Date: 2013-06-27 19:11:23 $ $Author: areeves $
Version number: $Revision: 1.4.20.1 $
Project: NAADSM and related applications
Website: http://www.naadsm.org
Author: Aaron Reeves <aaron.reeves@naadsm.org>
--------------------------------------------------
Copyright (C) 2007 Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)

interface

// FIX ME: Rename this struct to something more appropriate.

type RRemoteDatabaseParams = record
    useRemoteDatabase: boolean;
    remoteDatabaseHost: string;
    remoteDatabasePort: integer;
    scenarioID: integer;
    jobID: integer;
    randomSeed: integer;
  end
;

var
  remoteDBParams: RRemoteDatabaseParams;

implementation


initialization

  with remoteDBParams do
    begin
      useRemoteDatabase := false;
      remoteDatabaseHost := '0.0.0.0';
      remoteDatabasePort := 0;
      scenarioID := -1;
      jobID := -1;
      randomSeed := -1;
    end
  ;

end.