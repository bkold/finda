with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Dirent is

	function Open (Name : in String) return Directory_Type is
		function Open_C (Name : in char_array) return Directory_Type with
			Import, Convention=>C, Link_Name=>"opendir";

		Temp : aliased constant char_array := To_C(Name);
	begin
		return Open_C(Temp);
	end Open;


	function Name (Item : in Directory_Entry) return String is
		function C_Ada_Name (Item : in Directory_Entry) return chars_ptr
			with Import, Convention=>C, Link_Name=>"c_dirent_name";

		Name_Pointer : constant chars_ptr := C_Ada_Name(Item);
	begin
		return Value(Name_Pointer);
	end Name;

end Dirent;