with GNAT.Command_Line; use GNAT.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with System.Multiprocessors; use System.Multiprocessors;
with Ada.Text_IO; use Ada.Text_IO;
with Finder;

procedure Main is
	Pattern : Unbounded_String := Null_Unbounded_String;
	Depth : Natural := Natural'Last;
	Thread_Count : CPU := Number_Of_CPUs/2;
begin
	loop
		case Getopt ("e= d= t= -help") is
			when 'e' =>
				Pattern := To_Unbounded_String(Parameter);
			when 'd' =>
				Depth := Natural'Value(Parameter);
			when 't' =>
				Thread_Count := CPU'Value(Parameter);
			when '-' =>
				if Full_Switch = "-help" then
					Put_Line(Standard_Error,
						"Usage: finda [path...] [-e=pattern] [-t=thread_number] [-d=depth_number]");
					New_Line(Standard_Error, 1);
					Put_Line(Standard_Error,
						"default depth number is system's max standard integer value; " &
						"default thread number is half of the total number of cpus; " &
						"there is no default path;");
					New_Line(Standard_Error, 1);
					return;
				end if;
			when others =>
				exit;
		end case;
	end loop;

	if Pattern = Null_Unbounded_String then
		Put_Line(Standard_Error, "No pattern was set");
		return;
	end if;

	declare
		subtype CPU_Subrange is CPU range 1 .. Thread_Count;
		package F is new Finder(CPU_Subrange);
	begin
		F.Find_Start(Get_Argument, To_String(Pattern), Depth);
	end;
end Main;