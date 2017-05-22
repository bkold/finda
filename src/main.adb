with GNAT.Command_Line; use GNAT.Command_Line;
with System.Multiprocessors; use System.Multiprocessors;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories;
with GNAT.Regexp;
with Finder;

procedure Main is
	Depth : Natural := Natural'Last;
	Thread_Count : CPU := Number_Of_CPUs/2;
	Match_Token : GNAT.Regexp.Regexp;
	Got_Regex : Boolean := False;
	Pretty_Print : Boolean := False;
	Use_Full_Path : Boolean := False;

begin

	Parse_Argunents:begin
		loop
			case Getopt ("e= d= t= p f -help") is
				when 'e' =>
					Match_Token := GNAT.Regexp.Compile(Parameter, True);
					Got_Regex := True;
				when 'd' =>
					Depth := Natural'Value(Parameter);
				when 't' =>
					Thread_Count := CPU'Value(Parameter);
				when 'p' =>
					Pretty_Print := True;
				when 'f' =>
					Use_Full_Path := True;
				when '-' =>
					if Full_Switch = "-help" then
						Put_Line(Standard_Error,
							"Usage: finda [paths...] [-e=pattern] [-t=thread_number] [-d=depth_number] [-p] [-f]");
						New_Line(Standard_Error, 1);
						Put_Line(Standard_Error,
							"default depth number is system's max standard integer value; " &
							"default thread number is half of the total number of cpus; " &
							"default printing scheme is plain; " &
							"default path printing scheme is relative; " &
							"there is no default path;");
						New_Line(Standard_Error, 1);
						return;
					end if;
				when others =>
					exit;
			end case;
		end loop;
	exception
		when Invalid_Switch =>
			Put_Line(Standard_Error, "Invalid switch received '" & Full_Switch & "'");
			return;
		when Constraint_Error =>
			Put(Standard_Error, "Input for '" & Full_Switch & "' out of valid range ");
			if Full_Switch = "d" then Put_Line(Standard_Error, "of" & Natural'Image(Natural'First) & " ..." & Natural'Image(Natural'Last));
			elsif Full_Switch = "t" then Put_Line(Standard_Error, "of" & CPU'Image(CPU'First) & " ..." & CPU'Image(CPU'Last));
			end if;
			return;
		when GNAT.Regexp.Error_In_Regexp =>
			Put_Line(Standard_Error, "There was an error with the regular expression");
			return;
	end Parse_Argunents;

	Verify_All_Parameters:begin
		if not Got_Regex then
			Put_Line(Standard_Error, "No pattern was set");
			return;
		end if;
	end Verify_All_Parameters;

	Iterate_Given_Directories:declare
		subtype CPU_Subrange is CPU range 1 .. Thread_Count;
		package F is new Finder(CPU_Subrange, Depth, Match_Token, Pretty_Print);
	begin
		loop
			declare
				New_Directory : constant String := Get_Argument;
			begin
				exit when New_Directory = "";
				if Use_Full_Path then
					F.Find_Start(Ada.Directories.Full_Name(New_Directory));
				else
					F.Find_Start(New_Directory);
				end if;
			end;
		end loop;
	end Iterate_Given_Directories;

end Main;