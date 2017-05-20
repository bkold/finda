with GNAT.Command_Line; use GNAT.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;
-- with System.Microprocessors; use System.Microprocessors;
with Finder;

procedure Main is
	type CPU is range 1 .. 4;
	Pattern : Unbounded_String := Null_Unbounded_String;
	Depth : Natural := Natural'Last;
	Thread_Count : CPU := CPU'Last/2;
begin
	loop
		case Getopt ("e= d= t=") is
			when 'e' =>
				Pattern := To_Unbounded_String(Parameter);
			when 'd' =>
				Depth := Natural'Value(Parameter);
			when 't' =>
				Thread_Count := CPU'Value(Parameter);
			when others =>
				exit;
		end case;
	end loop;

	if Pattern = Null_Unbounded_String then
		Ada.Text_IO.Put_Line("Pattern not set");
		return;
	end if;

	declare
		subtype CPU_Subrange is CPU range 1 .. Thread_Count;
		package F is new Finder(CPU_Subrange);
	begin
		F.Find_Start(Get_Argument, To_String(Pattern), Depth);
	end;
end Main;