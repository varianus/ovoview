{
This file is part of OvoView
Copyright (C) 2018 Marco Caselli


OvoPlayer is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

}
unit AppConsts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLVersion, versionresource;

const
  DisplayAppName = 'OvoView';
  AppVersion = '0.0.1'; {.$i version.inc}
  ovoRevision = '1';
  BuildDate = {$I %DATE%};
  lazVersion  = lcl_version;         // Lazarus version (major.minor.micro)
//  lazRevision = RevisionStr;         // Lazarus SVN revision
  fpcVersion  = {$I %FPCVERSION%};   // FPC version (major.minor.micro)
  TargetCPU   = {$I %FPCTARGETCPU%}; // Target CPU of FPC
  TargetOS    = {$I %FPCTARGETOS%};  // Target Operating System of FPC


  AppName  = 'ovoview';
  DefaultResourceDirectory = '/usr/share/' + AppName + '/';
  ResourceSubDirectory     = 'Resources';

Resourcestring
  rVersionString = 'Version: %0:s (SVN Rev.: %1:s Date %2:s)';
  rBuildEnv = 'Build environment: Lazarus %0:s  FPC %1:s';
  rCurrentEngine = 'ImageMagick version: %s';

implementation

Function GetAppName : String;
begin
  Result:=AppName;
end;

initialization
 OnGetApplicationName := @GetAppName;


end.
