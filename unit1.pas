unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, sqldb, sqlite3conn, FileUtil, Forms, Controls,
  Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    DataSource1: TDataSource;
    Label3: TLabel;
    txtNew: TEdit;
    txtOld: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    SQLite3Connection1: TSQLite3Connection;
    SQLQuery1: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    txtPass: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    procedure createTable;
    procedure setReKey;
    procedure setUserVersion;
    procedure setAppId;
    { private declarations }
  public
    { public declarations }
  const
    APPID = '277843001';
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  newFile : Boolean;
begin

  SQLiteLibraryName := 'sqlite3.dll';


  SQLite3Connection1.Close; // Ensure the connection is closed when we start

  SQLite3Connection1.Password := txtPass.Text;

  SQLite3Connection1.DatabaseName := 'new.db'; // Set the database path
  //Sqlite3Dataset1.FileName := 'new.db'; // Set the database path
  try

    newFile := not FileExists(SQLite3Connection1.DatabaseName); // Check that the file doesn't already exist

    if newFile then
    begin

      // Make the database and the tables
      try
        SQLite3Connection1.Open;
        SQLTransaction1.Active := True;

        createTable;
        setUserVersion;
        setAppId;


        SQLTransaction1.Commit;
        //SQLTransaction1.Active := False;
        //SQLite3Connection1.Close;
      except
        ShowMessage(' ');
      end;

    end;

  except
    ShowMessage(' ');
  end;


end;

procedure TForm1.Button2Click(Sender: TObject);
var
  newFile : Boolean;
begin

  SQLiteLibraryName := 'sqlite3.dll';

  SQLite3Connection1.Close; // Ensure the connection is closed when we start

  SQLite3Connection1.Password := txtOld.Text;

  SQLite3Connection1.DatabaseName := 'new.db'; // Set the database path
  //Sqlite3Dataset1.FileName := 'new.db'; // Set the database path
  try

    // Make the database and the tables
    try
      SQLite3Connection1.Open;
      SQLTransaction1.Active := True;

      setReKey;


      SQLTransaction1.Commit;
      //SQLTransaction1.Active := False;
      //SQLite3Connection1.Close;
    except
      ShowMessage(' ');
    end;

  except
    ShowMessage(' ');
  end;
end;

procedure TForm1.createTable;
begin
  SQLite3Connection1.ExecuteDirect('CREATE TABLE "ENTRIES"('+
                    ' "id" Integer NOT NULL PRIMARY KEY AUTOINCREMENT,'+
                    ' "DTActual" DateTime NOT NULL,'+
                    ' "DTIntended" DateTime NOT NULL,'+
                    ' "Checksum" Text NOT NULL,'+
                    ' "Username" Text NOT NULL,'+
                    ' "Category" Text,'+
                    ' "UUID" Text NOT NULL,'+
                    ' "Entry" Text NOT NULL);');
end;

procedure TForm1.setUserVersion;
begin
  SQLite3Connection1.ExecuteDirect('PRAGMA user_version = ' + IntToStr(Random(2000000000)) + ';');
end;

procedure TForm1.setAppId;
begin
  SQLite3Connection1.ExecuteDirect('PRAGMA application_id = ' + APPID + ';');
end;

procedure TForm1.setReKey;
begin
  SQLite3Connection1.ExecuteDirect('PRAGMA rekey = ' + txtNew.Text + ';');
end;


end.

