with GNAT.Command_Line; use GNAT.Command_Line;
with System.Multiprocessors; use System.Multiprocessors;
with Ada.Text_IO; use Ada.Text_IO;
with Dirent.Writing_Schemes; use Dirent.Writing_Schemes;
with Ada.Directories;
with GNAT.Regexp;
with Finder;

procedure Main is
	subtype CPU_Local is CPU range 1 .. Number_Of_CPUs;

	Designated_Printer : Write_Procedure_Pointer := Write_Plain'access;
	Depth              : Natural := Natural'Last;
	Thread_Count       : CPU_Local := Number_Of_CPUs;
	Match_Token        : GNAT.Regexp.Regexp;
	Got_Regex          : Boolean := False;
	Use_Full_Path      : Boolean := False;
	Match_Full_Path    : Boolean := False;

	Help_Text : constant String :=
		"Usage: finda [paths...] [-e=pattern] [-t:thread_number] [-d:depth_number] [-p:scheme] [-a] [-f]" & ASCII.LF &
		ASCII.LF &
		"default depth number is system's max standard integer value; " &
		"default thread number is the total number of cpus; " &
		"default printing scheme is plain; " &
		"default path printed is relative; " &
		"default matching scheme is 'file name'; " &
		"there is no default path;" & ASCII.LF &
		ASCII.LF;

	Help_Rest : constant String :=
		"Swithes" & ASCII.LF &
		ASCII.LF &
		ASCII.HT & "a -- prints the full path, if used" & ASCII.LF &
		ASCII.HT & "f -- matches against the full path, can be used with 'a' or else relative path will be used" & ASCII.LF &
		ASCII.HT & "d -- sets depth level" & ASCII.LF &
		ASCII.HT & "t -- sets thread count, for optimal performance use your total CPU count" & ASCII.LF &
		ASCII.HT & "p -- determines the printing scheme" & ASCII.LF &
		ASCII.HT & ASCII.HT & "0 -- prints to one line enclosing each entry in quotation marks; use for xargs" & ASCII.LF &
		ASCII.HT & ASCII.HT & "1 -- prints to one line enclosing each entry in quotation marks" & ASCII.LF &
		ASCII.HT & ASCII.HT & "2 -- prints one entry to a line, colors according to file type; use when printing to terminal" & ASCII.LF &
		ASCII.HT & ASCII.HT & "3 -- (Default) prints one entry to a line, no added characters" & ASCII.LF &
		ASCII.LF;

begin

	Parse_Argunents:begin
		loop
			case Getopt ("e= d: t: p: a f -help") is
				when 'e' =>
					Match_Token := GNAT.Regexp.Compile(Parameter, True);
					Got_Regex := True;
				when 'd' =>
					Depth := Natural'Value(Parameter);
				when 't' =>
					Thread_Count := CPU_Local'Value(Parameter);
				when 'p' =>
					case Natural'Value(Parameter) is
					when 0 =>
						Designated_Printer := Write_Quotes_One'access;
					when 1 =>
						Designated_Printer := Write_Quotes'access;
					when 2 =>
						Designated_Printer := Write_Colors'access;
					when 3 =>
						Designated_Printer := Write_Plain'access;
					when others => raise Constraint_Error;
					end case;
				when 'a' =>
					Use_Full_Path := True;
				when 'f' =>
					Match_Full_Path := True;
				when '-' =>
					if Full_Switch = "-help" then
						Put_Line(Standard_Error, Help_Text);
						Put_Line(Standard_Error, Help_Rest);
						return;
					end if;
				when others =>
					exit;
			end case;
		end loop;
	exception
		when Invalid_Switch =>
			Put_Line(Standard_Error, "Invalid switch received '" & Full_Switch & "'");
			Put_Line(Standard_Error, Help_Text);
			return;
		when Constraint_Error =>
			Put(Standard_Error, "Input for '" & Full_Switch & "' out of valid range ");
			if Full_Switch = "d" then Put_Line(Standard_Error, "of" & Natural'Image(Natural'First) & " ..." & Natural'Image(Natural'Last));
			elsif Full_Switch = "t" then Put_Line(Standard_Error, "of" & CPU_Local'Image(CPU_Local'First) & " ..." & CPU_Local'Image(CPU_Local'Last));
			elsif Full_Switch = "p" then Put_Line(Standard_Error, "of" & Natural'Image(0) & " ..." & Natural'Image(3));
			end if;
			Put_Line(Standard_Error, Help_Text);
			return;
		when GNAT.Regexp.Error_In_Regexp =>
			Put_Line(Standard_Error, "There was an error with the regular expression");
			Put_Line(Standard_Error, Help_Text);
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
		package F is new Finder(CPU_Subrange, Depth, Match_Token, Match_Full_Path, Designated_Printer.all);
	begin
		loop
			declare
				New_Directory : constant String := Get_Argument;
			begin
				exit when New_Directory'Length = 0;
				if Use_Full_Path then
					F.Find_Start(Ada.Directories.Full_Name(New_Directory));
				else
					F.Find_Start(New_Directory);
				end if;
			end;
		end loop;
	end Iterate_Given_Directories;

end Main;