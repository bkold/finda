with GNAT.Command_Line; use GNAT.Command_Line;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings; use Ada.Strings;
with Finder;

procedure Main is
	Pattern : String (1 .. 50);
	Depth : Natural := Natural'Last;
begin
	loop
		case Getopt ("e= d=") is
			when 'e' =>
				Move(Parameter, Pattern);
			when 'd' =>
				Depth := Natural'Value(Parameter);
			when others =>
				exit;
		end case;
	end loop;

	Finder.Find_Start(Get_Argument, Trim(Pattern, Both), Depth);
end Main;