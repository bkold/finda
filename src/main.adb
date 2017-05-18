with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Command_Line; use GNAT.Command_Line;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings; use Ada.Strings;
with Finder;

procedure Main is
	Pattern : String (1 .. 50);
	Mode : Finder.Search_Mode;
begin
	loop
		case Getopt ("e= p=") is
			when 'e' =>
				-- Put_Line ("Seen -e with arg=" & Parameter);
				Move(Parameter, Pattern);
				Mode := Finder.Regular_Expression;
			when 'p' =>
				-- Put_Line ("Seen -e with arg=" & Parameter);
				Move(Parameter, Pattern);
				Mode := Finder.Plain;
			when others =>
				exit;
		end case;
	end loop;

	Finder.Find_Start(Get_Argument, Trim(Pattern, Both), Mode);
end Main;