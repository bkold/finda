with Dirent; use Dirent;
with Ada.Text_IO;
with Ada.Strings;
with Ada.Strings.Fixed;

package body Finder is

	procedure Find_Start (
		Directory, Token : in String;
		Desired_Depth : in Natural := Natural'Last)
	is
	begin
		Max_Depth := Desired_Depth;
		Match_Token := GNAT.Regexp.Compile(Token, True);
		Find(Directory, 0);
	end;

	procedure Find (
		Directory : in String;
		Depth : in Natural)
	is
		Local_Search : Directory_Type;
		D_Entry : Directory_Entry;
	begin
		Local_Search := Open(Directory);
		if Local_Search /= Null_Directory then
			loop
				D_Entry := Read(Local_Search);
				exit when D_Entry = Null_Entry;

				declare
					Entry_Name : constant String := Name(D_Entry);
					Thread_Check_Return : Integer;
				begin
					if GNAT.Regexp.Match(Entry_Name, Match_Token) then
						Write(Directory & '/' & Entry_Name);
					end if;

					if Depth < Max_Depth then
						if Mode(D_Entry) = DIR and
						then Entry_Name /= ".." and then Entry_Name /= "." then
							Task_Pool_Status.Check(Thread_Check_Return);
							case (Thread_Check_Return) is
								when 1 => Thread_1.Run(Directory & '/' & Entry_Name, Depth+1);
								when 2 => Thread_2.Run(Directory & '/' & Entry_Name, Depth+1);
								when 3 => Thread_3.Run(Directory & '/' & Entry_Name, Depth+1);
								when others => Find(Directory & '/' & Entry_Name, Depth+1);
							end case;
						end if;
					end if;
				end;
			end loop;

			Close(Local_Search);
		end if;
	end Find;

	procedure Write (Directory : in String) is
	begin
		Ada.Text_IO.Put_Line(Directory);
	end Write;

	task body Thread is
		use Ada.Strings;
		use Ada.Strings.Fixed;
		Directory : String(1 .. 200);
		Local_Depth : Natural;
	begin
		loop
			select
				accept Run (D : in String; Depth : in Natural) do
					Move(D, Directory);
					Local_Depth := Depth;
				end Run;
				Find(Trim(Directory, Both), Local_Depth);
				Task_Pool_Status.End_Thread(Num);
			or
				terminate;
			end select;
		end loop;
	end Thread;

	protected body Task_Pool_Status is
		procedure Check (Value : out Integer) is
		begin
			if    Status(1) then Value := 1; Status(1) := False;
			elsif Status(2) then Value := 2; Status(2) := False;
			elsif Status(3) then Value := 3; Status(3) := False;
			else Value := 0;
			end if;
		end Check;

		procedure End_Thread (Thread_Num : in Integer) is
		begin
			Status(Thread_Num) := True;
		end End_Thread;
	end Task_Pool_Status;

end Finder;