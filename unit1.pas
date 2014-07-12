unit Unit1;

// For this test application, I wanted to very simply try the following
// capabilities which I'll be using in a large application:
// - Creation of a SQLite3 Database
// - Creation of a database table
// - Setting various database metadata (PRAGMA)
// - Optionally encrypt the database using a key
// - Change (or set if not initially set) the encryption key for the database

// The application makes a new database file "new.db" within the local directory



// Using the following link, you can find a few options for versions
// of SQLite that provide support for encryption. Since I'm working
// mainly on Windows, I opted for the Open Source System.Data.SQLite
// http://wiki.freepascal.org/sqlite#Support_for_SQLite_encryption

// I used the "Precompiled Binaries for 32-bit Windows (.NET Framework 3.5 SP1)"
// and renamed SQLite.Interop.dll to sqlite3.dll, but you should be able to
// use any version you want as long as you have the required dependancies.
// http://system.data.sqlite.org/index.html/doc/trunk/www/downloads.wiki

// Make sure the sqlite3.dll is in the same directory as your application, or you
// *will* have errors, and your application will not work.



// MORE HELPFUL SQLITE INFO

// To read more about the SQLite Encryption Extension (SEE),
// use the following URL (Section: How To Compile And Use SEE)
// http://www.sqlite.org/see/doc/trunk/www/index.wiki

// Details about the SQLite File Format (and particularly about
// the Database Header) can be found at:
// http://www.sqlite.org/fileformat2.html#database_header

// Information about the various database PRAGMA (metadata) statements
// can be found at:
// http://www.sqlite.org/pragma.html


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
    { private declarations }
  public
    { public declarations }
  const
    // More information on the use of these values is below.
    // They need not be set as constants in your application. They can be any valid value
    application_id = 1189021115; // must be a 32-bit Unsigned Integer (Longword 0 .. 4294967295)
    user_version = 23400001;  // must be a 32-bit Signed Integer (LongInt -2147483648 .. 2147483647)

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

  SQLite3Connection1.Close; // Ensure the connection is closed when we start


  // Set the password initially.
  // Could probably be done with a PRAGMA statement, but this is so much simpler
  // and once set, doesn't need to be reset every time we open the database.
  // txtPass can be left blank if you want an unencrypted database.
  SQLite3Connection1.Password := txtPass.Text;

  SQLite3Connection1.DatabaseName := 'new.db'; // Set the path to the database

  try

    // Since we're making this database for the first time,
    // check whether the file already exists
    newFile := not FileExists(SQLite3Connection1.DatabaseName);

    if newFile then
    begin

      // Make the database and the tables
      try
        SQLite3Connection1.Open;
        SQLTransaction1.Active := True;


        // Per the SQLite Documentation (edited for clarity):
        // The pragma user_version is used to set or get the value of the user-version.
        // The user-version is a big-endian 32-bit signed integer stored in the database header at offset 60.
        // The user-version is not used internally by SQLite. It may be used by applications for any purpose.
        // http://www.sqlite.org/pragma.html#pragma_schema_version
        SQLite3Connection1.ExecuteDirect('PRAGMA user_version = ' + IntToStr(user_version) + ';');


        // Per the SQLite Documentation:
        // The application_id PRAGMA is used to query or set the 32-bit unsigned big-endian
        // "Application ID" integer located at offset 68 into the database header.
        // Applications that use SQLite as their application file-format should set the
        // Application ID integer to a unique integer so that utilities such as file(1) can
        // determine the specific file type rather than just reporting "SQLite3 Database".
        // A list of assigned application IDs can be seen by consulting the magic.txt file
        // in the SQLite source repository. 
        // http://www.sqlite.org/pragma.html#pragma_application_id
        SQLite3Connection1.ExecuteDirect('PRAGMA application_id = ' + IntToStr(application_id) + ';');


        // Here we're setting up a table named "USERS" in the new database
        SQLite3Connection1.ExecuteDirect('CREATE TABLE "USERS"('+
                    ' "id" Integer NOT NULL PRIMARY KEY AUTOINCREMENT,'+
                    ' "DTCreated" DateTime NOT NULL,'+
                    ' "Username" Text NOT NULL,'+
                    ' "PasswordSalt" Text NOT NULL,'+
                    ' "PasswordHash" Text NOT NULL,'+
                    ' "SecurityQues1" Text,'+
                    ' "SecurityAnsw1" Text);');


        SQLTransaction1.Commit;

      except
        ShowMessage('Unable to Create new Database');
      end;

    end;

  except
    ShowMessage('Unable to check if database file exists');
  end;


end;

procedure TForm1.Button2Click(Sender: TObject);
var
  newFile : Boolean;
begin

  SQLiteLibraryName := 'sqlite3.dll';

  SQLite3Connection1.Close; // Ensure the connection is closed when we start

  SQLite3Connection1.Password := txtOld.Text; // The current password

  SQLite3Connection1.DatabaseName := 'new.db'; // Set the path to the database

  // Update the database key
  try
    SQLite3Connection1.Open;
    SQLTransaction1.Active := True;


    // Here we change the key.
    // We use double-quotes here so that a blank key (IE: "") can be provided if
    // you want to remove encryption from the database.
    // This is a very simplistic demonstration. Ideally, we would take a stronger cryptographic approach
    // Some helpful info on this topic can be found at:
    // https://www.owasp.org/index.php/Cheat_Sheets
    // Per SQLite Documentation:
    // Note that the hexkey, rekey and hexrekey pragmas only work with SQLite version 3.6.8 and later.
    // http://www.sqlite.org/see/doc/trunk/www/readme.wiki
    // Section: Using the "key" PRAGMA
    SQLite3Connection1.ExecuteDirect('PRAGMA rekey = "' + txtNew.Text + '";');


    SQLTransaction1.Commit;
    SQLTransaction1.Active := False;
    SQLite3Connection1.Close;


    // Update the SQLite3Connection with the new password for future database transactions
    SQLite3Connection1.Password := txtNew.Text;


  except
    ShowMessage('Unable to set the new key using: PRAGMA rekey = ' + txtNew.Text + ';');
  end;

end;

end.

