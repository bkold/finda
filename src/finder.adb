with Dirent; use Dirent;
with Ada.Text_IO;
with Ada.Strings.Unbounded;

package body Finder is

	procedure Find_Start (
		Directory, Token : in String;
		Desired_Depth : in Natural := Natural'Last)
	is
		Thread_Check_Return : Thread_Access;
	begin
		Max_Depth := Desired_Depth;
		Match_Token := GNAT.Regexp.Compile(Token, True);
		for I in CPU'Range loop
			Threads(I) := new Thread(I);
		end loop;
		Task_Pool_Status.Check(Thread_Check_Return);
		Thread_Check_Return.Run(Directory, 0);
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
					Thread_Check_Return : Thread_Access;
				begin
					if GNAT.Regexp.Match(Entry_Name, Match_Token) then
						Write(Directory & '/' & Entry_Name);
					end if;

					if Depth < Max_Depth then
						if Mode(D_Entry) = DIR and
						then Entry_Name /= ".." and then Entry_Name /= "." then
							Task_Pool_Status.Check(Thread_Check_Return);
							if Thread_Check_Return /= Null then
								Thread_Check_Return.Run(Directory & '/' & Entry_Name, Depth+1);
							else
								Find(Directory & '/' & Entry_Name, Depth+1);
							end if;
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
		use Ada.Strings.Unbounded;
		Directory : Unbounded_String;
		Local_Depth : Natural;
	begin
		loop
			select
				accept Run (D : in String; Depth : in Natural) do
					Directory := To_Unbounded_String(D);
					Local_Depth := Depth;
				end Run;
				Find(To_String(Directory), Local_Depth);
				Task_Pool_Status.End_Thread(Num);
			or
				terminate;
			end select;
		end loop;
	end Thread;

	protected body Task_Pool_Status is
		procedure Check (Thread_Pointer : out Thread_Access) is
		begin
			for I in Status'Range loop
				if Status(I) = Ready then
					Status(I) := Working;
					Thread_Pointer := Threads(I);
					return;
				end if;
			end loop;
			Thread_Pointer := Null;
		end Check;

		procedure End_Thread (Thread_Num : in CPU) is
		begin
			Status(Thread_Num) := Ready;
		end End_Thread;
	end Task_Pool_Status;

end Finder;