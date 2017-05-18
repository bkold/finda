package Dirent is
	pragma Preelaborate(Dirent);

	type I_No is mod 2**64;

	type Directory_Type is private;

	Null_Directory : constant Directory_Type;

	type Directory_Entry is private;

	Null_Entry : constant Directory_Entry;

	type Directory_Mode is private;

	UNKNOWN : constant Directory_Mode;
	FIFO    : constant Directory_Mode;
	CHR     : constant Directory_Mode;
	DIR     : constant Directory_Mode;
	BLK     : constant Directory_Mode;
	REG     : constant Directory_Mode;
	LNK     : constant Directory_Mode;
	SOCK    : constant Directory_Mode;
	WHT     : constant Directory_Mode;

	function Close (Item : in Directory_Type) return Integer;

	function Open (Name : in String) return Directory_Type;

	function Read (Item : in Directory_Type) return Directory_Entry;

	function Read_R (Item : in Directory_Type; Dir : in Directory_Entry; New_Dir : in out Directory_Entry) return Integer;

	procedure Rewind (Item : in Directory_Type);

	procedure Seek (Item : in Directory_Type; Destination : in Long_Integer);

	function Tell (Item : in Directory_Type) return Long_Integer;

	function Name (Item : in Directory_Entry) return String;

	function Number (Item : in Directory_Entry) return I_No;

	function Mode (Item : in Directory_Entry) return Directory_Mode;

private

	type Void_Ptr is mod 2**64;

	type Directory_Type is new Void_Ptr;
	Null_Directory : constant Directory_Type := 0;

	type Directory_Entry is new Void_Ptr;
	Null_Entry : constant Directory_Entry := 0;

	type Directory_Mode is new Integer
		with Static_Predicate => Directory_Mode in 0 | 1 | 2 | 4 | 6 | 8 | 10 | 12 | 14 ;

	UNKNOWN : constant Directory_Mode := 0;
	FIFO    : constant Directory_Mode := 1;
	CHR     : constant Directory_Mode := 2;
	DIR     : constant Directory_Mode := 4;
	BLK     : constant Directory_Mode := 6;
	REG     : constant Directory_Mode := 8;
	LNK     : constant Directory_Mode := 10;
	SOCK    : constant Directory_Mode := 12;
	WHT     : constant Directory_Mode := 14;

	pragma Import(C, Close,  "closedir");
	pragma Import(C, Read,   "readdir");
	pragma Import(C, Read_R, "readdir_r");
	pragma Import(C, Rewind, "rewinddir");
	pragma Import(C, Seek,   "seekdir");
	pragma Import(C, Tell,   "telldir");
	pragma Import(C, Number, "c_dirent_number");
	pragma Import(C, Mode,   "c_dirent_mode");

end Dirent;