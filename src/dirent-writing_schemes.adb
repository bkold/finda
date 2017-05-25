with Ada.Text_IO;

package body Dirent.Writing_Schemes is
	pragma Warnings (Off, "formal parameter ""Mode"" is not referenced");

	procedure Write_Plain (Directory : in String; Mode : in Directory_Mode) is
	begin
		Ada.Text_IO.Put_Line(Directory);
	end Write_Plain;

	procedure Write_Quotes (Directory : in String; Mode : in Directory_Mode) is
	begin
		Ada.Text_IO.Put_Line("""" & Directory & """");
	end Write_Quotes;

	procedure Write_Quotes_One (Directory : in String; Mode : in Directory_Mode) is
	begin
		Ada.Text_IO.Put("""" & Directory & """ ");
	end Write_Quotes_One;

	procedure Write_Colors (Directory : in String; Mode : in Directory_Mode) is
		Green : constant String := ASCII.esc & "[92m";
		Blue : constant String := ASCII.esc & "[96m";
		Default : constant String := ASCII.esc & "[39m";
	begin
		if Mode = DIR then Ada.Text_IO.Put_Line(Green & Directory & Default);
		elsif Mode = LNK then Ada.Text_IO.Put_Line(Blue & Directory & Default);
		else Ada.Text_IO.Put_Line(Directory);
		end if;
	end Write_Colors;

end Dirent.Writing_Schemes;